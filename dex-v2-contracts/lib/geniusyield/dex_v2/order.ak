// SPDX-License-Identifier: BSL-1.1
// Copyright              : Copyright (c) 2025 GYELD GmbH
// License                : Business Source License 1.1
// Licensed Work          : GeniusYield Onchain Scripts
// Change License         : GPL v2.0 or later (after Change Date)
// See LICENSE file for full terms

use aiken/cbor.{serialise}
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{
  ScriptHash, VerificationKey, VerificationKeyHash, verify_ed25519_signature,
}
use aiken/interval
use aiken/math
use aiken/option
use aiken/primitive/bytearray
use cardano/address.{Address, Credential, Script}
use cardano/assets.{
  AssetName, Lovelace, PolicyId, Value, ada_asset_name, ada_policy_id,
}
use cardano/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, OutputReference, ValidityRange,
}
use geniusyield/dex_v2/address.{validate_address} as gy_address
use geniusyield/dex_v2/option.{assert} as gy_option
use geniusyield/dex_v2/types/assets as gy_assets
use geniusyield/dex_v2/types/multisig.{validate_multisig_script}
use geniusyield/dex_v2/types/order.{
  ConfigurationDatum, DeltaOracle, Fixed, OneWayDatum,
  OracleFillDetails, OrderDatum, Price, PriceDelta, PriceTimestamp,
  validate_asset_details, validate_price, validate_time_interval,
}
use geniusyield/dex_v2/types/rational.{Rational}
use geniusyield/dex_v2/types/sorted_list.{SortedList}

fn oracle_freshness_ms(freshness_seconds: Int) -> Int {
  math.max(freshness_seconds, 0) * 1_000
}

/// Public helper to validate oracle timestamp window against a given
/// transaction upper bound (freshness-only policy).
/// - Enforces freshness: timestamp >= (upper_bound - oracle_freshness_ms(freshness_seconds)).
///   The upper bound must be finite (checked by the caller).
pub fn oracle_timestamp_valid(
  upper_bound: Int,
  _upper_inclusive: Bool,
  price_ts: Int,
  freshness_seconds: Int,
) -> Bool {
  price_ts >= upper_bound - oracle_freshness_ms(freshness_seconds)
}

fn net_from_gross(gross: Int, ratio: Rational) -> Int {
  if gross <= 0 {
    gross
  } else {
    let numerator = ratio.numerator
    let denominator = ratio.denominator
    let total = denominator + numerator
    if total <= 0 {
      gross
    } else {
      (gross * denominator + total - 1) / total
    }
  }
}

fn ceil_mul_ratio(amount: Int, ratio: Rational) -> Int {
  if amount <= 0 {
    0
  } else {
    let numerator = ratio.numerator
    let denominator = ratio.denominator
    if numerator <= 0 {
      0
    } else if denominator <= 0 {
      amount
    } else {
      (amount * numerator + denominator - 1) / denominator
    }
  }
}

fn floor_mul_ratio(amount: Int, ratio: Rational) -> Int {
  if amount <= 0 {
    0
  } else {
    let numerator = ratio.numerator
    let denominator = ratio.denominator
    if numerator <= 0 || denominator <= 0 {
      0
    } else {
      (amount * numerator) / denominator
    }
  }
}

/// Populate the NFT set corresponding to given output reference & starting count. Note that given `count` should be greater than zero.
pub fn populate_nft_set(
  output_reference: OutputReference,
  count: Int,
  nft_set: Dict<AssetName, Int>,
) -> Dict<AssetName, Int> {
  // NFT name scheme: sha256(txId || ix) truncated to 28 bytes,
  // then prefix a 1-byte counter to support multi-mint (<256 enforced).
  let base =
    bytearray.push(
      output_reference.transaction_id,
      output_reference.output_index,
    )
  do_populate_nft_set(base, count, nft_set)
}

fn do_populate_nft_set(
  output_reference_bytes: ByteArray,
  count: Int,
  nft_set: Dict<AssetName, Int>,
) -> Dict<AssetName, Int> {
  if count == 0 {
    nft_set
  } else if count > 0 {
    let digest = crypto.sha2_256(output_reference_bytes)
    // NOTE: slice end is inclusive; 0..27 yields 28 bytes
    let base28 = bytearray.slice(digest, 0, 27)
    let nft = bytearray.push(base28, count)
    do_populate_nft_set(
      output_reference_bytes,
      count - 1,
      dict.insert(nft_set, nft, 1),
    )
  } else {
    fail @"count cannot be negative"
  }
}

/// (wrapper) verify outputs in minting validator
// NB: outputs corresponding to new orders should be sorted relative to NFTs (to allow validator to be efficient)
pub fn verify_outputs(
  outputs: List<Output>,
  nft_set: Dict<AssetName, Int>,
  nft_policy_id: PolicyId,
  config: ConfigurationDatum,
  spend_hash: ScriptHash,
) -> Bool {
  let ordered = dict.keys(nft_set) // get sorted list of NFTs from unordered set
  do_verify_outputs(
    outputs,
    ordered,
    nft_policy_id,
    config,
    spend_hash,
  )
  // NB: removed fee_total argument because flat fee check already made before
}

fn do_verify_outputs(
  outputs: List<Output>,
  nft_set: List<AssetName>,
  nft_policy_id: PolicyId,
  config: ConfigurationDatum,
  spend_hash: ScriptHash,
) -> Bool {
  when outputs is {
    [] -> list.is_empty(nft_set) // all outputs verified

    [out, ..outs] -> {
      // NB: once again remove flat fee check after fee check earlier (UTXO indexer)
      // filter all NFTs in that UTXO originating from `policy_id`
      let tokens = out.value |> assets.tokens(nft_policy_id)

      when nft_set is {
        // all NFTs exhausted or no NFTs present at all
        []            -> do_verify_outputs(outs, [], nft_policy_id, config, spend_hash)

        [nft, ..nfts] ->
          // if tokens contain unique NFT then `verify_output` and continue recursively
          if tokens == (dict.empty |> dict.insert(nft, 1)) {
            and {
              verify_output(out, nft_policy_id, nft, config, spend_hash),
              do_verify_outputs(outs, nfts, nft_policy_id, config, spend_hash),
            }
          }
          // else skip `verify_output` (non-relevant output)
          else {
            do_verify_outputs(outs, nft_set, nft_policy_id, config, spend_hash)
          }
      }
    }
  }
}

// verify singular `out` when fee check verified && `out` contains unique NFT from minting policy
fn verify_output(
  output: Output,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  config: ConfigurationDatum,
  spend_hash: ScriptHash,
) -> Bool {
  expect InlineDatum(datum_raw) = output.datum
  expect datum: OrderDatum = datum_raw // ensure `output` contains order datum
  expect Script(sh) = output.address.payment_credential // output goes to script (checked later to be same as `spend_hash`)
  // `output` contains (i) `a`, (ii) `b`, (iii) `min_deposit` (in ADA), (iv) the NFT (all values checked against datum stored)
  let required_value: Value =
    assets.zero
      |> assets.add(
          datum.a.asset_class.policy_id,
          datum.a.asset_class.asset_name,
          datum.a.amount,
        )
      |> assets.add(
          datum.b.asset_class.policy_id,
          datum.b.asset_class.asset_name,
          datum.b.amount,
        )
      |> assets.add(
          ada_policy_id,
          ada_asset_name,
          config.min_deposit,
        )
      |> assets.add(nft_policy_id, nft_asset_name, 1)
  and {

    // TODO: Check how the costs increase in case of checking geq. Normal equality is efficient. Strict equality might just be better to ensure some invariants when filling/updating the order.
    // TODO: Check for exhaustiveness wrt old validator.
    output.value == required_value,
    // I don't check in multisig validation that there is at least one credential, I think that's not required.
    validate_multisig_script(datum.owner),
    validate_address(datum.payment_addr),
    sh == spend_hash,
    datum.nft_name == nft_asset_name,
    validate_asset_details(datum.a), // order amount contains non-negative amount for `a`
    validate_asset_details(datum.b),
    !gy_assets.is_equal(datum.a.asset_class, datum.b.asset_class), // `a` and `b` are different from each other
    validate_price(datum.price, datum.a, datum.b), // structural and numeric sanity checks on price
    validate_time_interval(datum.validity_start, datum.validity_end),
    datum.taker_lovelace_flat_fee == config.taker_lovelace_flat_fee,
    datum.taker_fee_ratio == config.taker_fee_ratio,
    datum.maker_fee_ratio == config.maker_fee_ratio,
    datum.oracle_freshness_seconds == config.oracle_freshness_seconds,
    datum.oracle_freshness_seconds >= 0,
  }
}

pub fn comp_percent_maker_fee(amount: Int, ratio: Rational) -> Int {
  amount * ratio.numerator / ratio.denominator
}

fn is_one_way(price: Price) -> Bool {
  when price is {
    Fixed { reverse, .. } -> option.is_none(reverse)
    DeltaOracle { reverse, .. } -> option.is_none(reverse)
  }
}

fn adjust_delta(price: Rational, PriceDelta { offset, spread }: PriceDelta) {
  rational.add(rational.one, spread)
    |> rational.mul(price)
    |> rational.add(offset)
}

fn oracle_price(
  oracle_fill_details: OracleFillDetails,
  oracle_public_key: VerificationKey,
  expected_base: gy_assets.AssetClass,
  expected_quote: gy_assets.AssetClass,
  tx_validity_range: ValidityRange,
  freshness_seconds: Int,
) -> Rational {
  if verify_ed25519_signature(
    oracle_public_key,
    serialise(oracle_fill_details.price_msg),
    oracle_fill_details.price_signature,
  ) {
    let price_msg = oracle_fill_details.price_msg
    if !gy_assets.is_equal(price_msg.base_asset, expected_base) {
      fail @"oracle base asset mismatch"
    } else if !gy_assets.is_equal(price_msg.quote_asset, expected_quote) {
      fail @"oracle quote asset mismatch"
    } else {
      let ub = tx_validity_range.upper_bound
      let price_timestamp = price_msg.timestamp
      when ub.bound_type is {
        interval.Finite(hi) ->
          if oracle_timestamp_valid(
            hi,
            ub.is_inclusive,
            price_timestamp,
            freshness_seconds,
          ) {
            price_msg.price
          } else {
            fail @"stale oracle price timestamp"
          }
        _ -> fail @"tx validity interval doesn't have a finite upper bound"
      }
    }
  } else {
    fail @"invalid oracle signature"
  }
}

/// Code is similar to standard library's `is_entirely_after`, but includes the given point as well.
fn is_after(self: interval.Interval<Int>, point: Int) -> Bool {
  when self.lower_bound.bound_type is {
    interval.Finite(low) ->
      if self.lower_bound.is_inclusive {
        point <= low
      } else {
        point < low
      }
    _ -> False
  }
}

/// Code is similar to standard library's `is_entirely_before`, but includes the given point as well.
pub fn is_before(self: interval.Interval<Int>, point: Int) -> Bool {
  when self.upper_bound.bound_type is {
    interval.Finite(hi) ->
      if self.upper_bound.is_inclusive {
        hi <= point
      } else {
        hi < point
      }
    _ -> False
  }
}

fn order_can_be_filled(
  order_validity_start: Option<Int>,
  order_validity_end: Option<Int>,
  tx_validity_range: ValidityRange,
) -> Bool {
  when (order_validity_start, order_validity_end) is {
    (Some(start), Some(end)) -> and {
        is_after(tx_validity_range, start),
        is_before(tx_validity_range, end),
      }
    (Some(start), None) -> is_after(tx_validity_range, start)
    (None, Some(end)) -> is_before(tx_validity_range, end)
    (None, None) -> True
  }
}

/// Case where we are filling multiple orders in a single transaction. We assume that the subsequence of input orders in the list of `Input` correspond to subsequence of the "relevant" outputs in the `Output` list. Thus off-chain transaction builder would need to ensure that order output ordering is in sync with order input ordering.
pub fn traverse_fill_inputs_outputs(
  inputs: List<Input>,
  outputs: List<Output>,
  nft_policy_id: PolicyId,
  config: ConfigurationDatum,
  burn_mint: Dict<AssetName, Int>,
  // TODO: `burn_asset_names` could have been just sorted list of asset names...
  burn_asset_names: Dict<AssetName, Int>,
  percent_taker_acc: Value,
  percent_maker_acc: Value,
  taker_flat_due: Bool,
  max_taker_lovelace_flat_fee: Int,
  fee_addr: Address,
  ref_inputs: List<Input>,
  validity_range: ValidityRange,
  oracle_prices: Dict<AssetName, OracleFillDetails>,
) -> Bool {
  when inputs is {
    // No more inputs to check.
    [] -> {
      let taker_value =
        percent_taker_acc
          |> assets.reduce(
              assets.zero,
              fn(policy_id, asset_name, amt, acc) {
                if amt > 0 {
                  acc |> assets.add(policy_id, asset_name, amt)
                } else {
                  acc
                }
              },
            )
      let maker_value = percent_maker_acc
      let is_taker_fee_req = !(taker_value == assets.zero)
      let require_taker_flat =
        (is_taker_fee_req || taker_flat_due)
          && max_taker_lovelace_flat_fee > 0
      and {
        burn_mint == burn_asset_names,
        // TODO: To note require fee output if total fees turn out to be zero?
        // Check fee output from remaining outputs, thus fee output should be after relevant outputs.
        list.any(
          outputs,
          fn(out) {
            and {
              out.address == fee_addr,
              out.datum == InlineDatum(nft_policy_id),
              assets.match(
                out.value,
                assets.merge(taker_value, maker_value)
                  |> assets.add(
                      assets.ada_policy_id,
                      assets.ada_asset_name,
                      if require_taker_flat {
                        max_taker_lovelace_flat_fee
                      } else {
                        0
                      },
                    ),
                >=,
              ),
            }
          },
        ),
      }
    }
    [in, ..ins] -> {
      let in_value_under_nft = in.output.value |> assets.tokens(nft_policy_id)
      // This input is not relevant.
      if in_value_under_nft == dict.empty {
          traverse_fill_inputs_outputs(
            ins,
            outputs,
            nft_policy_id,
            config,
            burn_mint,
            burn_asset_names,
          percent_taker_acc,
          percent_maker_acc,
          taker_flat_due,
          max_taker_lovelace_flat_fee,
          fee_addr,
          ref_inputs,
          validity_range,
          oracle_prices,
        )
      } else {
        // Found relevant input.
        expect [nft_asset_name] = dict.keys(in_value_under_nft)
        expect InlineDatum(in_datum_raw) = in.output.datum
        expect in_datum: OrderDatum = in_datum_raw
        expect
          order_can_be_filled(
            in_datum.validity_start,
            in_datum.validity_end,
            validity_range,
          )
        when
          traverse_fill_outputs(
            in.output.address,
            in.output.value,
            in_datum,
            outputs,
            nft_policy_id,
            nft_asset_name,
            config,
            percent_taker_acc,
            percent_maker_acc,
            is_one_way(in_datum.price),
            in_datum.oracle_freshness_seconds,
            validity_range,
            oracle_prices,
          )
        is {
          None -> False
          Some((
            is_burn,
            remaining_outputs,
            new_percent_taker_acc,
            new_percent_maker_acc,
            did_consume,
          )) ->
            traverse_fill_inputs_outputs(
              ins,
              remaining_outputs,
              nft_policy_id,
              config,
              burn_mint,
              if is_burn {
              dict.insert(burn_asset_names, nft_asset_name, -1)
            } else {
              burn_asset_names
            },
              new_percent_taker_acc,
              new_percent_maker_acc,
              taker_flat_due || did_consume,
              math.max(
                max_taker_lovelace_flat_fee,
                in_datum.taker_lovelace_flat_fee,
              ),
              fee_addr,
              ref_inputs,
              validity_range,
              oracle_prices,
            )
        }
      }
    }
  }
}

/// Traverse the list of outputs to find the relevant satisfying output corresponding to this input. If such an output is found, returns the remaining output list along with a boolean to inform whether the order was burnt or not and updated `percent_taker_acc`, else we return `None` to signify failure.
pub fn traverse_fill_outputs(
  input_address: Address,
  input_value: Value,
  input_datum: OrderDatum,
  outputs: List<Output>,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  config: ConfigurationDatum,
  percent_taker_acc: Value,
  percent_maker_acc: Value,
  one_way: Bool,
  freshness_seconds: Int,
  validity_range: ValidityRange,
  oracle_prices: Dict<AssetName, OracleFillDetails>,
) -> Option<(Bool, List<Output>, Value, Value, Bool)> {
  when outputs is {
    // We didn't find corresponding output.
    [] -> None
    [out, ..outs] -> {
      let out_value_under_nft = out.value |> assets.tokens(nft_policy_id)
      // TODO: `is_relevant` could have been `Int` to save some costs, benchmark it. Atleast make it a new data type.
      let is_relevant =
        if out_value_under_nft == dict.empty {
          // If the order is completely filled (which is allowed only in one-way case), we need to check for the output going to the owner with inlined datum representing token name of the NFT.
          if one_way {
            when out.datum is {
              NoDatum | DatumHash(_) -> None
              InlineDatum(out_datum_raw) ->
                if out_datum_raw is out_datum: OneWayDatum {
                  if out_datum.nft_asset_name == nft_asset_name {
                    Some(True)
                  } else {
                    None
                  }
                } else {
                  None
                }
            }
          } else {
            None
          }
        } else {
          expect [out_nft_asset_name] = dict.keys(out_value_under_nft)
          if out_nft_asset_name == nft_asset_name {
            Some(False)
          } else {
            None
          }
        }
      when is_relevant is {
        None ->
          traverse_fill_outputs(
            input_address,
            input_value,
            input_datum,
            outs,
            nft_policy_id,
            nft_asset_name,
            config,
            percent_taker_acc,
            percent_maker_acc,
            one_way,
            freshness_seconds,
            validity_range,
            oracle_prices,
          )
        // This is the case where the order is burnt, i.e., one-way order is completely consumed.
        Some(True) -> {
          expect InlineDatum(out_datum_raw) = out.datum
          expect _out_datum_refined: OneWayDatum = out_datum_raw
          let price_opt =
            when input_datum.price is {
              Fixed { straight, .. } -> Some(straight)
              DeltaOracle { oracle_key, straight, .. } ->
                when dict.get(oracle_prices, nft_asset_name) is {
                  None -> None
                  Some(oracle_fill_details) ->
                    Some(
                      oracle_price(
                        oracle_fill_details,
                        oracle_key,
                        input_datum.a.asset_class,
                        input_datum.b.asset_class,
                        validity_range,
                        freshness_seconds,
                      )
                        |> adjust_delta(straight),
                    )
                }
            }
          when price_opt is {
            None -> None
            Some(price) -> {
          let offer_consumed = input_datum.a.amount
          let offer_net = net_from_gross(offer_consumed, input_datum.maker_fee_ratio)
          let payment_provided = ceil_mul_ratio(offer_net, price)
          let taker_fee_payment =
            floor_mul_ratio(payment_provided, input_datum.taker_fee_ratio)
          let taker_fee_offer =
            floor_mul_ratio(offer_net, input_datum.taker_fee_ratio)
          let offer_asset = input_datum.a.asset_class
          let payment_asset = input_datum.b.asset_class
          let maker_fee_offer = offer_consumed - offer_net
          let owner_value =
            assets.zero
              |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
              |> assets.add(
                  input_datum.b.asset_class.policy_id,
                  input_datum.b.asset_class.asset_name,
                  input_datum.b.amount + payment_provided,
                )
          if assets.match(out.value, owner_value, ==) {
            Some(
              (
                True,
                outs,
                percent_taker_acc
                  |> assets.add(
                      payment_asset.policy_id,
                      payment_asset.asset_name,
                      taker_fee_payment,
                    )
                  |> assets.add(
                      offer_asset.policy_id,
                      offer_asset.asset_name,
                      -taker_fee_offer,
                    ),
                percent_maker_acc
                  |> assets.add(
                      input_datum.a.asset_class.policy_id,
                      input_datum.a.asset_class.asset_name,
                      maker_fee_offer,
                    ),
                offer_consumed > 0 || payment_provided > 0,
              ),
            )
          } else {
            None
      }
            }
          }
        }
        // This is the case where there is a continuing output.
        Some(False) -> {
         expect InlineDatum(out_datum_raw) = out.datum
         expect out_datum: OrderDatum = out_datum_raw
         let is_a_consumed = out_datum.a.amount < input_datum.a.amount
          let price_opt =
            when input_datum.price is {
              Fixed { straight, reverse } ->
                if is_a_consumed {
                  Some(straight)
                } else {
                  Some(assert(reverse))
                }
              DeltaOracle { oracle_key, straight, reverse } ->
                when dict.get(oracle_prices, nft_asset_name) is {
                  None -> None
                  Some(oracle_fill_details) -> {
                    let (expected_base, expected_quote, delta) =
                      if is_a_consumed {
                        (
                          input_datum.a.asset_class,
                          input_datum.b.asset_class,
                          straight,
                        )
                      } else {
                        (
                          input_datum.b.asset_class,
                          input_datum.a.asset_class,
                          assert(reverse),
                        )
                      }
                    Some(
                      oracle_price(
                        oracle_fill_details,
                        oracle_key,
                        expected_base,
                        expected_quote,
                        validity_range,
                        freshness_seconds,
                      )
                        |> adjust_delta(delta),
                    )
                  }
                }
           }
          let next =
            when price_opt is {
              None -> None
              Some(price) -> {
          let maker_ratio = input_datum.maker_fee_ratio
          let taker_ratio = input_datum.taker_fee_ratio
          let offer_consumed =
            if is_a_consumed {
              input_datum.a.amount - out_datum.a.amount
            } else {
              input_datum.b.amount - out_datum.b.amount
            }
          let (offer_asset, payment_asset) =
            if is_a_consumed {
              (input_datum.a.asset_class, input_datum.b.asset_class)
            } else {
              (input_datum.b.asset_class, input_datum.a.asset_class)
            }
          let offer_net = net_from_gross(offer_consumed, maker_ratio)
          let payment_provided = ceil_mul_ratio(offer_net, price)
          let maker_fee_offer = offer_consumed - offer_net
          let taker_fee_payment =
            floor_mul_ratio(payment_provided, taker_ratio)
          let taker_fee_offer =
            floor_mul_ratio(offer_net, taker_ratio)
          let new_a_amount =
            if is_a_consumed {
              input_datum.a.amount - offer_consumed
            } else {
              input_datum.a.amount + payment_provided
            }
          let new_b_amount =
            if is_a_consumed {
              input_datum.b.amount + payment_provided
            } else {
              input_datum.b.amount - offer_consumed
            }
          let required_base =
            assets.zero
              |> assets.add(nft_policy_id, nft_asset_name, 1)
              |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
          let required_value =
            required_base
              |> assets.add(
                  input_datum.a.asset_class.policy_id,
                  input_datum.a.asset_class.asset_name,
                  new_a_amount,
                )
              |> assets.add(
                  input_datum.b.asset_class.policy_id,
                  input_datum.b.asset_class.asset_name,
                  new_b_amount,
                )
          let sat = and {
              assets.match(out.value, required_value, ==),
              out.address == input_address,
              out_datum.owner == input_datum.owner,
              out_datum.payment_addr == input_datum.payment_addr,
              out_datum.nft_name == input_datum.nft_name,
              out_datum.price == input_datum.price,
              out_datum.validity_start == input_datum.validity_start,
              out_datum.validity_end == input_datum.validity_end,
              out_datum.taker_lovelace_flat_fee == input_datum.taker_lovelace_flat_fee,
              out_datum.taker_fee_ratio == input_datum.taker_fee_ratio,
              out_datum.maker_fee_ratio == input_datum.maker_fee_ratio,
              out_datum.oracle_freshness_seconds
                == input_datum.oracle_freshness_seconds,
              // Asset classes remain identical
              out_datum.a.asset_class == input_datum.a.asset_class,
              out_datum.b.asset_class == input_datum.b.asset_class,
              // Amount transitions reflect positive consumption on one side and payment on the other
             out_datum.a.amount == new_a_amount,
             out_datum.b.amount == new_b_amount,
           }
          if sat {
            Some(
              (
                False,
                outs,
                percent_taker_acc
                  |> assets.add(
                      payment_asset.policy_id,
                      payment_asset.asset_name,
                      taker_fee_payment,
                    )
                  |> assets.add(
                      offer_asset.policy_id,
                      offer_asset.asset_name,
                      -taker_fee_offer,
                    ),
                percent_maker_acc
                  |> assets.add(
                      offer_asset.policy_id,
                      offer_asset.asset_name,
                      maker_fee_offer,
                    ),
                offer_consumed > 0 || payment_provided > 0,
              ),
            )
          } else {
            None
          }
              }
            }
          next
        }
      }
    }
  }
}

/// Case where we are cancelling multiple orders in a single transaction. We assume that the subsequence of input orders in the list of `Input` correspond to subsequence of the "relevant" outputs in the `Output` list. Thus off-chain transaction builder would need to ensure that relevant output ordering is in sync with order input ordering.
pub fn traverse_cancel_inputs_outputs(
  inputs: List<Input>,
  outputs: List<Output>,
  nft_policy_id: PolicyId,
  extra_signatories: List<VerificationKeyHash>,
  burn_mint: Dict<AssetName, Int>,
  withdrawals: Pairs<Credential, Lovelace>,
  cancel_sigs: SortedList<VerificationKeyHash>,
  cancel_scripts: SortedList<ScriptHash>,
  // TODO: It could have been just sorted list of asset names...
  burn_asset_names: Dict<AssetName, Int>,
) -> Bool {
  when inputs is {
    // No more inputs to check.
    [] -> and {
        burn_mint == burn_asset_names,
        cancel_sigs
          |> sorted_list.to_list
          |> list.all(
              fn(cancel_sig) {
                list.any(
                  extra_signatories,
                  fn(extra_sig) { cancel_sig == extra_sig },
                )
              },
            ),
        cancel_scripts
          |> sorted_list.to_list
          |> list.all(
              fn(cancel_script) {
                list.any(
                  withdrawals,
                  fn(Pair(cred, _)) { cred == Script(cancel_script) },
                )
              },
            ),
      }
    [in, ..ins] -> {
      let in_value_under_nft = in.output.value |> assets.tokens(nft_policy_id)
      // This input is not relevant.
      if in_value_under_nft == dict.empty {
        traverse_cancel_inputs_outputs(
          ins,
          outputs,
          nft_policy_id,
          extra_signatories,
          burn_mint,
          withdrawals,
          cancel_sigs,
          cancel_scripts,
          burn_asset_names,
        )
      } else {
        // Found relevant input.
        expect [nft_asset_name] = dict.keys(in_value_under_nft)
        expect InlineDatum(in_datum_raw) = in.output.datum
        expect in_datum: OrderDatum = in_datum_raw
        when
          traverse_cancel_outputs(
            in.output.address,
            in.output.value,
            in_datum,
            outputs,
            nft_policy_id,
            nft_asset_name,
          )
        is {
          None -> False
          Some(remaining_outputs) ->
            traverse_cancel_inputs_outputs(
              ins,
              remaining_outputs,
              nft_policy_id,
              extra_signatories,
              burn_mint,
              withdrawals,
              list.foldl(
                in_datum.owner.signatures,
                cancel_sigs,
                fn(sig, acc) { sorted_list.insert(acc, sig, bytearray.compare) },
              ),
              list.foldl(
                in_datum.owner.scripts,
                cancel_scripts,
                fn(script, acc) {
                  sorted_list.insert(acc, script, bytearray.compare)
                },
              ),
              dict.insert(burn_asset_names, nft_asset_name, -1),
            )
        }
      }
    }
  }
}

/// Traverse the list of outputs to find the relevant satisfying output corresponding to this input. If such an output is found, returns the remaining output list.
pub fn traverse_cancel_outputs(
  input_address: Address,
  input_value: Value,
  input_datum: OrderDatum,
  outputs: List<Output>,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
) -> Option<List<Output>> {
  when outputs is {
    // We didn't find corresponding output.
    [] -> None
    [out, ..outs] -> {
      // We need to check for the output going to the owner with inlined datum representing token name of the NFT.
      let is_relevant = out.datum == InlineDatum(nft_asset_name)
      if !is_relevant {
        // Irrelevant output, we proceed to next one.
        traverse_cancel_outputs(
          input_address,
          input_value,
          input_datum,
          outs,
          nft_policy_id,
          nft_asset_name,
        )
      } else {
        let sat = and {
            // Output should be at payment address.
            out.address == input_datum.payment_addr,
            // Everything that is inside the UTxO modulo NFT should be returned. Though unlikely, we allow output to have additional lovelaces.
            assets.match(
              out.value
                |> assets.add(nft_policy_id, nft_asset_name, 1),
              input_value,
              >=,
            ),
          }
        if sat {
          Some(outs)
        } else {
          None
        }
      }
    }
  }
}
