// SPDX-License-Identifier: BSL-1.1
// Copyright              : Copyright (c) 2025 GYELD GmbH
// License                : Business Source License 1.1
// Licensed Work          : GeniusYield Onchain Scripts
// Change License         : GPL v2.0 or later (after Change Date)
// See LICENSE file for full terms

// use aiken/collection/list
use aiken/crypto
use aiken/primitive/bytearray
use cardano/address.{Address, Inline, Script, VerificationKey}
use cardano/assets.{ada_asset_name, ada_policy_id}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
  TransactionId,
}
use geniusyield/dex_v2/order as order_lib
use geniusyield/dex_v2/types/assets.{AssetClass} as gy_assets
use geniusyield/dex_v2/types/multisig.{MultisigScript}
use geniusyield/dex_v2/types/order.{
  AssetDetails, CancelOrder, ConfigurationDatum, DeltaOracle, FillRedeemer, Fixed,
  OrderDatum, OutputReferenceInt, Price, PriceDelta,
}
use geniusyield/dex_v2/types/rational.{Rational}
use order as order_val

fn mk_hash(size: Int, last_byte: Int, acc: ByteArray) -> ByteArray {
  expect size >= 0
  if size == 0 {
    acc |> bytearray.push(last_byte)
  } else {
    mk_hash(size - 1, last_byte, acc |> bytearray.push(0))
  }
}

fn mk_order_address(n: Int) -> Address {
  let vkh = mk_hash(27, n, #"")
  Address {
    payment_credential: Script(spend_hash),
    stake_credential: Some(Inline(VerificationKey(vkh))),
  }
}

fn mk_address(n: Int) -> Address {
  let vkh = mk_hash(27, n, #"")
  Address { payment_credential: VerificationKey(vkh), stake_credential: None }
}

// fn do_mk_addresses(n: Int, acc: List<Address>) -> List<Address> {
//   if n == 0 {
//     acc
//   } else {
//     do_mk_addresses(n - 1, list.push(acc, mk_address(n)))
//   }
// }

fn mk_tx_hash(n: Int) -> TransactionId {
  mk_hash(31, n, #"")
}

fn mk_output_reference(n: Int) -> OutputReference {
  OutputReference { transaction_id: mk_tx_hash(0), output_index: n }
}

fn mk_order_nft_name(output_reference: OutputReference) -> ByteArray {
  // sha256(txId || ix) truncated to 28 bytes, then append a 1-byte counter (1)
  let digest =
    bytearray.push(
      output_reference.transaction_id,
      output_reference.output_index,
    )
      |> crypto.sha2_256
  // NOTE: slice end is inclusive; 0..27 yields 28 bytes
  let base28 = bytearray.slice(digest, 0, 27)
  base28 |> bytearray.push(1)
}

fn mk_order_nft_name_i(output_reference: OutputReference, i: Int) -> ByteArray {
  let digest =
    bytearray.push(
      output_reference.transaction_id,
      output_reference.output_index,
    )
      |> crypto.sha2_256
  // 28-byte base + 1-byte counter i
  let base28 = bytearray.slice(digest, 0, 27)
  base28 |> bytearray.push(i)
}

// GENS.
const offer_policy_id =
  #"dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb"

const offer_asset_name = #"0014df1047454e53"

// USDM.
const payment_policy_id =
  #"c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad"

const payment_asset_name = #"0014df105553444d"

const order_nft_policy_id =
  #"00000000000000000000000000000000000000000000000000000001"

const fill_hash = #"00000000000000000000000000000000000000000000000000000002"

const cancel_hash = #"00000000000000000000000000000000000000000000000000000003"

const fee_addr =
  Address {
    payment_credential: VerificationKey(
      #"00000000000000000000000000000000000000000000000000000004",
    ),
    stake_credential: None,
  }

const spend_hash = #"00000000000000000000000000000000000000000000000000000005"

const config: ConfigurationDatum =
  ConfigurationDatum {
    total_signatories: [],
    required_signatories: 0,
    nft_policy_id: order_nft_policy_id,
    fill_hash,
    cancel_hash,
    fee_addr,
    maker_lovelace_flat_fee: 1_000_000,
    maker_fee_ratio: Rational { numerator: 3, denominator: 1000 },
    taker_lovelace_flat_fee: 1_000_000,
    taker_fee_ratio: Rational { numerator: 3, denominator: 1000 },
    oracle_freshness_seconds: 60,
    min_deposit: 3_000_000,
  }

const simple_nft: AssetClass =
  AssetClass {
    policy_id: #"00000000000000000000000000000000000000000000000000000008",
    asset_name: #"1234",
  }

const config_ref_input: Input =
  Input {
    output_reference: OutputReference {
      transaction_id: #"0000000000000000000000000000000000000000000000000000000000000006",
      output_index: 0,
    },
    output: Output {
      address: Address {
        payment_credential: VerificationKey(
          #"00000000000000000000000000000000000000000000000000000007",
        ),
        stake_credential: None,
      },
      value: assets.zero
        |> assets.add(ada_policy_id, ada_asset_name, 1_000_000_000)
        |> assets.add(simple_nft.policy_id, simple_nft.asset_name, 1),
      datum: InlineDatum(config),
      reference_script: None,
    },
  }

const a_asset_class: AssetClass =
  AssetClass { policy_id: offer_policy_id, asset_name: offer_asset_name }

const b_asset_class: AssetClass =
  AssetClass { policy_id: payment_policy_id, asset_name: payment_asset_name }

fn mk_high_input(n: Int) -> Input {
  let output_reference = mk_output_reference(n)
  let addr = mk_address(n)
  Input {
    output_reference,
    output: Output {
      address: addr,
      value: assets.zero
        |> assets.add(ada_policy_id, ada_asset_name, 1_000_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn mk_order_output(spent_output_reference: OutputReference, n: Int, i: Int) -> Output {
  mk_order_output_with_config(spent_output_reference, n, i, config)
}

fn mk_order_output_with_config(
  spent_output_reference: OutputReference,
  n: Int,
  i: Int,
  cfg: ConfigurationDatum,
) -> Output {
  let addr = mk_order_address(n)
  let offer_amount = 1_000_000 + n
  let nft_name = mk_order_nft_name_i(spent_output_reference, i)
  Output {
    address: addr,
    value: assets.zero
      |> assets.add(order_nft_policy_id, nft_name, 1)
      |> assets.add(
          ada_policy_id,
          ada_asset_name,
          cfg.min_deposit,
        )
      |> assets.add(
          offer_policy_id,
          offer_asset_name,
          offer_amount,
        ),
    datum: InlineDatum(
      mk_order_datum_with_config(
        addr,
        nft_name,
        offer_amount,
        cfg,
      ),
    ),
    reference_script: None,
  }
}

fn mk_order_datum_with_config(
  addr,
  nft_name,
  offer_amount,
  cfg: ConfigurationDatum,
) -> OrderDatum {
  OrderDatum {
    owner: MultisigScript { signatures: [], scripts: [] },
    payment_addr: addr,
    nft_name,
    a: AssetDetails { asset_class: a_asset_class, amount: offer_amount },
    b: AssetDetails { asset_class: b_asset_class, amount: 0 },
    price: Fixed { straight: rational.one, reverse: None },
    validity_start: None,
    validity_end: None,
    taker_lovelace_flat_fee: cfg.taker_lovelace_flat_fee,
    taker_fee_ratio: cfg.taker_fee_ratio,
    maker_fee_ratio: cfg.maker_fee_ratio,
    oracle_freshness_seconds: cfg.oracle_freshness_seconds,
  }
}

fn mk_fee_output_with_config(cfg: ConfigurationDatum, order_count: Int) -> Output {
  let total = order_count * cfg.maker_lovelace_flat_fee
  Output {
    address: cfg.fee_addr,
    value:
      assets.zero
        |> assets.add(ada_policy_id, ada_asset_name, total),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }
}

test assess_mint_costs() {
  let in1 = mk_high_input(0)
  let order_out = mk_order_output(in1.output_reference, 0, 1)
  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: // 1_000_000_000 ADA
      [config_ref_input],
      outputs: // order output + maker fee bundle
      [mk_fee_output_with_config(config, 1), order_out],
      mint: // 1_000_000 OfferedAsset, NFT, fees
      assets.zero
        // NFT minted
        |> assets.add(
            order_nft_policy_id,
            mk_order_nft_name(in1.output_reference),
            1,
          ),
    }
  let mint_redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 1 })
  let result =
    order_val.order_validator_mint.mint(
      simple_nft,
      spend_hash,
      mint_redeemer,
      order_nft_policy_id,
      transaction,
    )
  result
}

test mint_rejects_missing_fee_output() {
  let in1 = mk_high_input(2)
  let order_out = mk_order_output(in1.output_reference, 2, 1)
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: [config_ref_input],
      outputs: [order_out],
      mint:
        assets.zero
          |> assets.add(
              order_nft_policy_id,
              mk_order_nft_name(in1.output_reference),
              1,
            ),
    }
  let redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 1 })
  !order_val.order_validator_mint.mint(
    simple_nft,
    spend_hash,
    redeemer,
    order_nft_policy_id,
    tx,
  )
}

test mint_rejects_fee_output_wrong_datum() {
  let in1 = mk_high_input(3)
  let order_out = mk_order_output(in1.output_reference, 3, 1)
  let wrong_fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            ada_policy_id,
            ada_asset_name,
            config.maker_lovelace_flat_fee,
          ),
    datum: InlineDatum(spend_hash),
    reference_script: None,
  }
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: [config_ref_input],
      outputs: [wrong_fee_out, order_out],
      mint:
        assets.zero
          |> assets.add(
              order_nft_policy_id,
              mk_order_nft_name(in1.output_reference),
              1,
            ),
    }
  let redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 1 })
  !order_val.order_validator_mint.mint(
    simple_nft,
    spend_hash,
    redeemer,
    order_nft_policy_id,
    tx,
  )
}

test mint_rejects_insufficient_maker_fee_payment() {
  let in1 = mk_high_input(4)
  let order_out = mk_order_output(in1.output_reference, 4, 1)
  let underpay_fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            ada_policy_id,
            ada_asset_name,
            config.maker_lovelace_flat_fee - 1,
          ),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: [config_ref_input],
      outputs: [underpay_fee_out, order_out],
      mint:
        assets.zero
          |> assets.add(
              order_nft_policy_id,
              mk_order_nft_name(in1.output_reference),
              1,
            ),
    }
  let redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 1 })
  !order_val.order_validator_mint.mint(
    simple_nft,
    spend_hash,
    redeemer,
    order_nft_policy_id,
    tx,
  )
}

test mint_rejects_fee_output_wrong_index() {
  let in1 = mk_high_input(5)
  let order_out = mk_order_output(in1.output_reference, 5, 1)
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: [config_ref_input],
      outputs: [order_out, mk_fee_output_with_config(config, 1)], // wrongly indexed fee output
      mint:
        assets.zero
          |> assets.add(
              order_nft_policy_id,
              mk_order_nft_name(in1.output_reference),
              1,
            ),
    }
  let redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 1 })

  !order_val.order_validator_mint.mint(
    simple_nft,
    spend_hash,
    redeemer,
    order_nft_policy_id,
    tx,
  )
}

test mint_rejects_negative_freshness() {
  let bad_config =
    ConfigurationDatum { ..config, oracle_freshness_seconds: -1 }
  let bad_ref_input =
    Input {
      ..config_ref_input,
      output: Output { ..config_ref_input.output, datum: InlineDatum(bad_config) },
    }
  let in1 = mk_high_input(1)
  let offer_amount = 1_000_100
  let nft_name = mk_order_nft_name_i(in1.output_reference, 1)
  let addr = mk_order_address(1)
  let order_out =
    Output {
      address: addr,
      value: assets.zero
        |> assets.add(order_nft_policy_id, nft_name, 1)
        |> assets.add(
            ada_policy_id,
            ada_asset_name,
            bad_config.min_deposit,
          )
        |> assets.add(
            offer_policy_id,
            offer_asset_name,
            offer_amount,
          ),
      datum: InlineDatum(
        OrderDatum {
          owner: MultisigScript { signatures: [], scripts: [] },
          payment_addr: addr,
          nft_name,
          a: AssetDetails { asset_class: a_asset_class, amount: offer_amount },
          b: AssetDetails { asset_class: b_asset_class, amount: 0 },
          price: Fixed { straight: rational.one, reverse: None },
          validity_start: None,
          validity_end: None,
          taker_lovelace_flat_fee: bad_config.taker_lovelace_flat_fee,
          taker_fee_ratio: bad_config.taker_fee_ratio,
          maker_fee_ratio: bad_config.maker_fee_ratio,
          oracle_freshness_seconds: bad_config.oracle_freshness_seconds,
        },
      ),
      reference_script: None,
    }
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: [bad_ref_input],
      outputs: [mk_fee_output_with_config(bad_config, 1), order_out],
      mint:
        assets.zero
          |> assets.add(order_nft_policy_id, mk_order_nft_name(in1.output_reference), 1),
    }
  let redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 1 })
  !order_val.order_validator_mint.mint(
    simple_nft,
    spend_hash,
    redeemer,
    order_nft_policy_id,
    tx,
  )
}

test mint_accepts_updated_freshness() {
  let new_config =
    ConfigurationDatum { ..config, oracle_freshness_seconds: 120 }
  let new_ref_input =
    Input {
      ..config_ref_input,
      output: Output { ..config_ref_input.output, datum: InlineDatum(new_config) },
    }
  let in1 = mk_high_input(2)
  let order_out = mk_order_output_with_config(
    in1.output_reference,
    2,
    1,
    new_config,
  )
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: [new_ref_input],
      outputs: [mk_fee_output_with_config(new_config, 1), order_out],
      mint:
        assets.zero
          |> assets.add(order_nft_policy_id, mk_order_nft_name(in1.output_reference), 1),
    }
  let redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 1 })
  order_val.order_validator_mint.mint(
    simple_nft,
    spend_hash,
    redeemer,
    order_nft_policy_id,
    tx,
  )
}

fn mk_two_way_output(
  spent_output_reference: OutputReference,
  n: Int,
  i: Int,
  a_amt: Int,
  b_amt: Int,
  price: Price,
) -> Output {
  let addr = mk_order_address(n)
  let nft_name = mk_order_nft_name_i(spent_output_reference, i)
  Output {
    address: addr,
    value: assets.zero
      |> assets.add(order_nft_policy_id, nft_name, 1)
      |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
      |> assets.add(a_asset_class.policy_id, a_asset_class.asset_name, a_amt)
      |> assets.add(b_asset_class.policy_id, b_asset_class.asset_name, b_amt),
    datum: InlineDatum(
      OrderDatum {
        owner: MultisigScript { signatures: [], scripts: [] },
        payment_addr: addr,
        nft_name,
        a: AssetDetails { asset_class: a_asset_class, amount: a_amt },
        b: AssetDetails { asset_class: b_asset_class, amount: b_amt },
        price,
        validity_start: None,
        validity_end: None,
        taker_lovelace_flat_fee: config.taker_lovelace_flat_fee,
        taker_fee_ratio: config.taker_fee_ratio,
        maker_fee_ratio: config.maker_fee_ratio,
        oracle_freshness_seconds: config.oracle_freshness_seconds,
      }
    ),
    reference_script: None,
  }
}

fn percent(x: Int, r: Rational) -> Int {
  rational.from_int(x) |> rational.mul(r) |> rational.floor()
}

fn include_maker_fee(fill_target: Int, ratio: Rational) -> Int {
  include_maker_fee_iter(fill_target, ratio, fill_target)
}

fn include_maker_fee_iter(
  fill_target: Int,
  ratio: Rational,
  current: Int,
) -> Int {
  let fee = percent(current, ratio)
  let next = fill_target + fee
  if next == current {
    current
  } else {
    include_maker_fee_iter(fill_target, ratio, next)
  }
}

fn maker_fee_from_fill(fill_target: Int, ratio: Rational) -> Int {
  include_maker_fee(fill_target, ratio) - fill_target
}

fn payment_from_fill(fill_target: Int, price: Rational) -> Int {
  rational.from_int(fill_target)
    |> rational.mul(price)
    |> rational.ceil()
}

fn taker_fee_payment_from_fill(fill_target: Int, price: Rational, ratio: Rational) -> Int {
  percent(payment_from_fill(fill_target, price), ratio)
}

test mint_multiple_orders_mixed_types() {
  let in1 = mk_high_input(0)
  let a1 = 1_000_000
  let b2 = 2_000_000
  let a3 = 1_000_100
  let a4 = 1_000_200
  let b4 = 2_000_300
  // Four outputs: one-way fixed, two-way fixed, one-way delta, two-way delta
  let out1 =
    mk_two_way_output(
      in1.output_reference,
      0,
      1,
      a1,
      0,
      Fixed { straight: rational.one, reverse: None },
    )
  let out2 =
    mk_two_way_output(
      in1.output_reference,
      1,
      2,
      a1 + 1,
      b2 + 2,
      Fixed {
        straight: Rational { numerator: 2, denominator: 1 },
        reverse: Some(Rational { numerator: 1, denominator: 2 }),
      },
    )
  let out3 =
    mk_two_way_output(
      in1.output_reference,
      2,
      3,
      a3,
      0,
      DeltaOracle {
        oracle_key: #"abababababababababababababababababababababababababababababab",
        straight: PriceDelta {
          offset: Rational { numerator: 0, denominator: 1 },
          spread: Rational { numerator: 1, denominator: 10 },
        },
        reverse: None,
      },
    )
  let out4 =
    mk_two_way_output(
      in1.output_reference,
      3,
      4,
      a4,
      b4,
      DeltaOracle {
        oracle_key: #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd",
        straight: PriceDelta {
          offset: Rational { numerator: 0, denominator: 1 },
          spread: Rational { numerator: 1, denominator: 20 },
        },
        reverse: Some(PriceDelta {
          offset: Rational { numerator: 0, denominator: 1 },
          spread: Rational { numerator: 1, denominator: 25 },
        }),
      },
    )

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [in1],
      reference_inputs: [config_ref_input],
      outputs: [mk_fee_output_with_config(config, 4), out1, out2, out3, out4], // fee_output must be placed first
      mint:
        assets.zero
          |> assets.add(order_nft_policy_id, mk_order_nft_name_i(in1.output_reference, 1), 1)
          |> assets.add(order_nft_policy_id, mk_order_nft_name_i(in1.output_reference, 2), 1)
          |> assets.add(order_nft_policy_id, mk_order_nft_name_i(in1.output_reference, 3), 1)
          |> assets.add(order_nft_policy_id, mk_order_nft_name_i(in1.output_reference, 4), 1),
    }
  let mint_redeemer =
    Some(OutputReferenceInt { output_ref: in1.output_reference, amount: 4 })
  order_val.order_validator_mint.mint(
    simple_nft,
    spend_hash,
    mint_redeemer,
    order_nft_policy_id,
    transaction,
  )
}

test fill_one_way_fixed_partial_charges_maker_fee_per_fill() {
  let spent_ref = mk_output_reference(10)
  let minted_out = mk_two_way_output(
    spent_ref,
    30,
    1,
    50_000,
    0,
    Fixed { straight: rational.one, reverse: None },
  )
  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(20),
      output_index: 0,
    },
    output: minted_out,
  }
  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 10_000
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  expect Fixed { straight, .. } = od.price
  let payment_provided = payment_from_fill(fill_target, straight)
  let maker_fee_offer = maker_fee_from_fill(fill_target, config.maker_fee_ratio)
  let taker_fee_pay = taker_fee_payment_from_fill(fill_target, straight, config.taker_fee_ratio)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount - offer_consumed,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount + payment_provided,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount + payment_provided,
      },
    }),
    reference_script: None,
  }

  let fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            taker_fee_pay,
          )
        |> assets.add(ada_policy_id, ada_asset_name, od.taker_lovelace_flat_fee),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont, fee_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_one_way_delta_requires_oracle_details() {
  let spent_ref = mk_output_reference(70)
  let price_delta = PriceDelta {
    offset: Rational { numerator: 0, denominator: 1 },
    spread: Rational { numerator: 1, denominator: 100 },
  }
  let minted_out = mk_two_way_output(
    spent_ref,
    70,
    1,
    40_000,
    0,
    DeltaOracle {
      oracle_key: #"edededededededededededededededededededededededededededededed",
      straight: price_delta,
      reverse: None,
    },
  )
  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(70),
      output_index: 0,
    },
    output: minted_out,
  }
  expect InlineDatum(od_raw) = minted_out.datum
  expect _od: OrderDatum = od_raw

  // Attempting to fill without supplying oracle details must fail
  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [minted_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  !order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_rejects_missing_fee_output() {
  let spent_ref = mk_output_reference(60)
  let minted_out = mk_two_way_output(
    spent_ref,
    60,
    1,
    50_000,
    0,
    Fixed { straight: rational.one, reverse: None },
  )
  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(61),
      output_index: 0,
    },
    output: minted_out,
  }
  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 10_000
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  expect Fixed { straight, .. } = od.price
  let payment_provided = payment_from_fill(fill_target, straight)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount - offer_consumed,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount + payment_provided,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount + payment_provided,
      },
    }),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  !order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_accepts_partial_two_way_fixed() {
  let spent_ref = mk_output_reference(65)
  let straight_price = Rational { numerator: 5, denominator: 4 }
  let reverse_price = Rational { numerator: 4, denominator: 5 }
  let minted_out = mk_two_way_output(
    spent_ref,
    65,
    1,
    1_200,
    700,
    Fixed { straight: straight_price, reverse: Some(reverse_price) },
  )

  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(66),
      output_index: 0,
    },
    output: minted_out,
  }

  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 350
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  let payment_provided = payment_from_fill(fill_target, straight_price)
  let maker_fee_offer = maker_fee_from_fill(fill_target, config.maker_fee_ratio)
  let taker_fee_pay = percent(payment_provided, config.taker_fee_ratio)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount - offer_consumed,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount + payment_provided,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount + payment_provided,
      },
    }),
    reference_script: None,
  }

  let taker_addr = mk_address(42)
  let taker_out = Output {
    address: taker_addr,
    value:
      assets.zero
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            fill_target,
          ),
    datum: NoDatum,
    reference_script: None,
  }

  let fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            taker_fee_pay,
          )
        |> assets.add(ada_policy_id, ada_asset_name, od.taker_lovelace_flat_fee),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont, fee_out, taker_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_accepts_reverse_two_way_continuation() {
  let spent_ref = mk_output_reference(75)
  let straight_price = Rational { numerator: 5, denominator: 4 }
  let reverse_price = Rational { numerator: 4, denominator: 5 }
  let minted_out = mk_two_way_output(
    spent_ref,
    75,
    1,
    850,
    1_138,
    Fixed { straight: straight_price, reverse: Some(reverse_price) },
  )

  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(76),
      output_index: 0,
    },
    output: minted_out,
  }

  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 400
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  let payment_provided = payment_from_fill(fill_target, reverse_price)
  let maker_fee_offer = maker_fee_from_fill(fill_target, config.maker_fee_ratio)
  let taker_fee_pay = taker_fee_payment_from_fill(fill_target, reverse_price, config.taker_fee_ratio)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount + payment_provided,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount - offer_consumed,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount + payment_provided,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount - offer_consumed,
      },
    }),
    reference_script: None,
  }

  let taker_addr = mk_address(44)
  let taker_out = Output {
    address: taker_addr,
    value:
      assets.zero
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            fill_target,
          ),
    datum: NoDatum,
    reference_script: None,
  }

  let fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            taker_fee_pay,
          )
        |> assets.add(ada_policy_id, ada_asset_name, od.taker_lovelace_flat_fee),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont, fee_out, taker_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_rejects_reverse_two_way_continuation_balanced() {
  let straight_price = Rational { numerator: 5, denominator: 4 }
  let reverse_price = Rational { numerator: 4, denominator: 5 }
  let spent_ref = mk_output_reference(90)
  let minted_out = mk_two_way_output(
    spent_ref,
    90,
    1,
    1_200,
    700,
    Fixed { straight: straight_price, reverse: Some(reverse_price) },
  )

  let order_input = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(90),
      output_index: 0,
    },
    output: minted_out,
  }

  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 360
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  let payment_provided = payment_from_fill(fill_target, reverse_price)
  let maker_fee_offer = maker_fee_from_fill(fill_target, config.maker_fee_ratio)
  let taker_fee_payment = taker_fee_payment_from_fill(fill_target, reverse_price, config.taker_fee_ratio)

  let continuing_out = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount + payment_provided,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount - offer_consumed,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount + payment_provided,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount - offer_consumed + 1,
      },
    }),
    reference_script: None,
  }

  let fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            taker_fee_payment,
          )
        |> assets.add(ada_policy_id, ada_asset_name, config.taker_lovelace_flat_fee),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [order_input],
    reference_inputs: [config_ref_input],
    outputs: [continuing_out, fee_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  !order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_rejects_fee_output_wrong_datum() {
  let spent_ref = mk_output_reference(61)
  let minted_out = mk_two_way_output(
    spent_ref,
    61,
    1,
    55_000,
    0,
    Fixed { straight: rational.one, reverse: None },
  )
  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(62),
      output_index: 0,
    },
    output: minted_out,
  }
  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 8_000
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  expect Fixed { straight, .. } = od.price
  let payment_provided = payment_from_fill(fill_target, straight)
  let maker_fee_offer = maker_fee_from_fill(fill_target, config.maker_fee_ratio)
  let taker_fee_pay = taker_fee_payment_from_fill(fill_target, straight, config.taker_fee_ratio)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount - offer_consumed,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount + payment_provided,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount + payment_provided,
      },
    }),
    reference_script: None,
  }

  let wrong_fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            taker_fee_pay,
          )
        |> assets.add(ada_policy_id, ada_asset_name, od.taker_lovelace_flat_fee),
    datum: InlineDatum(spend_hash),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont, wrong_fee_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  !order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_rejects_straight_overcharging_payment() {
  let straight_price = Rational { numerator: 8, denominator: 5 }
  let reverse_price = Rational { numerator: 5, denominator: 8 }
  let spent_ref = mk_output_reference(90)
  let minted_out =
    mk_two_way_output(
      spent_ref,
      90,
      1,
      100_000_000_000,
      0,
      Fixed { straight: straight_price, reverse: Some(reverse_price) },
    )
  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(91),
      output_index: 0,
    },
    output: minted_out,
  }
  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 10_000_000_000
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  let payment_provided_wrong =
    rational.from_int(offer_consumed)
      |> rational.mul(straight_price)
      |> rational.ceil()
  let maker_fee_offer = percent(offer_consumed, config.maker_fee_ratio)
  let taker_fee_pay = percent(payment_provided_wrong, config.taker_fee_ratio)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount - offer_consumed,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount + payment_provided_wrong,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount + payment_provided_wrong,
      },
    }),
    reference_script: None,
  }

  let fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            taker_fee_pay,
          )
        |> assets.add(
            ada_policy_id,
            ada_asset_name,
            od.taker_lovelace_flat_fee,
          ),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let taker_addr = mk_address(95)
  let taker_out = Output {
    address: taker_addr,
    value:
      assets.zero
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            offer_consumed - maker_fee_offer,
          ),
    datum: NoDatum,
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont, fee_out, taker_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  !order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_rejects_reverse_overcharging_payment() {
  let straight_price = Rational { numerator: 5, denominator: 4 }
  let reverse_price = Rational { numerator: 4, denominator: 5 }
  let spent_ref = mk_output_reference(92)
  let minted_out =
    mk_two_way_output(
      spent_ref,
      92,
      1,
      750_000_000,
      200_000_000_000,
      Fixed { straight: straight_price, reverse: Some(reverse_price) },
    )
  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(93),
      output_index: 0,
    },
    output: minted_out,
  }
  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 40_000_000_000
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  let payment_provided_wrong =
    rational.from_int(offer_consumed)
      |> rational.mul(reverse_price)
      |> rational.ceil()
  let maker_fee_offer = percent(offer_consumed, config.maker_fee_ratio)
  let taker_fee_pay = percent(payment_provided_wrong, config.taker_fee_ratio)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount + payment_provided_wrong,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount - offer_consumed,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount + payment_provided_wrong,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount - offer_consumed,
      },
    }),
    reference_script: None,
  }

  let fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            taker_fee_pay,
          )
        |> assets.add(
            ada_policy_id,
            ada_asset_name,
            od.taker_lovelace_flat_fee,
          ),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let taker_addr = mk_address(96)
  let taker_out = Output {
    address: taker_addr,
    value:
      assets.zero
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            fill_target,
          ),
    datum: NoDatum,
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont, fee_out, taker_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  !order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test fill_rejects_insufficient_fee_output_value() {
  let spent_ref = mk_output_reference(62)
  let minted_out = mk_two_way_output(
    spent_ref,
    62,
    1,
    60_000,
    0,
    Fixed { straight: rational.one, reverse: None },
  )
  let in_order = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(63),
      output_index: 0,
    },
    output: minted_out,
  }
  expect InlineDatum(od_raw) = minted_out.datum
  expect od: OrderDatum = od_raw

  let fill_target = 12_000
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  expect Fixed { straight, .. } = od.price
  let payment_provided = payment_from_fill(fill_target, straight)
  let maker_fee_offer = maker_fee_from_fill(fill_target, config.maker_fee_ratio)
  let taker_fee_pay = taker_fee_payment_from_fill(fill_target, straight, config.taker_fee_ratio)

  let out_cont = Output {
    address: minted_out.address,
    value:
      assets.zero
        |> assets.add(order_nft_policy_id, od.nft_name, 1)
        |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            od.a.amount - offer_consumed,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            od.b.amount + payment_provided,
          ),
    datum: InlineDatum(OrderDatum {
      ..od,
      a: AssetDetails {
        asset_class: od.a.asset_class,
        amount: od.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od.b.asset_class,
        amount: od.b.amount + payment_provided,
      },
    }),
    reference_script: None,
  }

  let under_fee_out = Output {
    address: fee_addr,
    value:
      assets.zero
        |> assets.add(
            od.a.asset_class.policy_id,
            od.a.asset_class.asset_name,
            maker_fee_offer,
          )
        |> assets.add(
            od.b.asset_class.policy_id,
            od.b.asset_class.asset_name,
            taker_fee_pay,
          )
        |> assets.add(
            ada_policy_id,
            ada_asset_name,
            od.taker_lovelace_flat_fee - 1,
          ),
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order],
    reference_inputs: [config_ref_input],
    outputs: [out_cont, under_fee_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  !order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

test taker_fee_cancel_out_two_leg_route() {
  let spent_ref = mk_output_reference(11)

  // Order 1: offers A, requires B
  let order_ab = mk_two_way_output(
    spent_ref,
    40,
    1,
    120_000,
    60_000,
    Fixed { straight: rational.one, reverse: Some(rational.one) },
  )

  // Order 2: offers B, requires A (manual swap of asset classes)
  let nft_ba = mk_order_nft_name_i(spent_ref, 2)
  let order_ba = Output {
    address: mk_order_address(41),
    value: assets.zero
      |> assets.add(order_nft_policy_id, nft_ba, 1)
      |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
      |> assets.add(b_asset_class.policy_id, b_asset_class.asset_name, 120_000)
      |> assets.add(a_asset_class.policy_id, a_asset_class.asset_name, 60_000),
    datum: InlineDatum(
      OrderDatum {
        owner: MultisigScript { signatures: [], scripts: [] },
        payment_addr: mk_order_address(41),
        nft_name: nft_ba,
        a: AssetDetails { asset_class: b_asset_class, amount: 120_000 },
        b: AssetDetails { asset_class: a_asset_class, amount: 60_000 },
        price: Fixed { straight: rational.one, reverse: Some(rational.one) },
        validity_start: None,
        validity_end: None,
        taker_lovelace_flat_fee: config.taker_lovelace_flat_fee,
        taker_fee_ratio: config.taker_fee_ratio,
        maker_fee_ratio: config.maker_fee_ratio,
        oracle_freshness_seconds: config.oracle_freshness_seconds,
      }
    ),
    reference_script: None,
  }

  let in_ab = Input {
    output_reference: OutputReference { transaction_id: mk_tx_hash(50), output_index: 0 },
    output: order_ab,
  }
  let in_ba = Input {
    output_reference: OutputReference { transaction_id: mk_tx_hash(51), output_index: 1 },
    output: order_ba,
  }

  let fill_target = 10_000
  let offer_consumed = include_maker_fee(fill_target, config.maker_fee_ratio)
  let maker_fee = offer_consumed - fill_target
  let payment_provided = fill_target

  // Continuation for order AB
  expect InlineDatum(od_ab_raw) = order_ab.datum
  expect od_ab: OrderDatum = od_ab_raw
  let out_ab_cont = Output {
    address: order_ab.address,
    value: assets.zero
      |> assets.add(order_nft_policy_id, od_ab.nft_name, 1)
      |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
      |> assets.add(
          od_ab.a.asset_class.policy_id,
          od_ab.a.asset_class.asset_name,
          od_ab.a.amount - offer_consumed,
        )
      |> assets.add(
          od_ab.b.asset_class.policy_id,
          od_ab.b.asset_class.asset_name,
          od_ab.b.amount + payment_provided,
        ),
    datum: InlineDatum(OrderDatum {
      ..od_ab,
      a: AssetDetails {
        asset_class: od_ab.a.asset_class,
        amount: od_ab.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od_ab.b.asset_class,
        amount: od_ab.b.amount + payment_provided,
      },
    }),
    reference_script: None,
  }

  // Continuation for order BA
  expect InlineDatum(od_ba_raw) = order_ba.datum
  expect od_ba: OrderDatum = od_ba_raw
  let out_ba_cont = Output {
    address: order_ba.address,
    value: assets.zero
      |> assets.add(order_nft_policy_id, od_ba.nft_name, 1)
      |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
      |> assets.add(
          od_ba.a.asset_class.policy_id,
          od_ba.a.asset_class.asset_name,
          od_ba.a.amount - offer_consumed,
        )
      |> assets.add(
          od_ba.b.asset_class.policy_id,
          od_ba.b.asset_class.asset_name,
          od_ba.b.amount + payment_provided,
        ),
    datum: InlineDatum(OrderDatum {
      ..od_ba,
      a: AssetDetails {
        asset_class: od_ba.a.asset_class,
        amount: od_ba.a.amount - offer_consumed,
      },
      b: AssetDetails {
        asset_class: od_ba.b.asset_class,
        amount: od_ba.b.amount + payment_provided,
      },
    }),
    reference_script: None,
  }

  // Fee output: maker fees for both assets, taker percentage cancels out, flat applied once
  let fee_value = assets.zero
    |> assets.add(a_asset_class.policy_id, a_asset_class.asset_name, maker_fee)
    |> assets.add(b_asset_class.policy_id, b_asset_class.asset_name, maker_fee)
    |> assets.add(ada_policy_id, ada_asset_name, config.taker_lovelace_flat_fee)
  let fee_out = Output {
    address: fee_addr,
    value: fee_value,
    datum: InlineDatum(order_nft_policy_id),
    reference_script: None,
  }

  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_ab, in_ba],
    reference_inputs: [config_ref_input],
    outputs: [out_ab_cont, out_ba_cont, fee_out],
    mint: assets.zero,
    withdrawals: [Pair(Script(fill_hash), 0)],
  }

  order_val.order_validator_stake_fill.withdraw(
    simple_nft,
    FillRedeemer { oracle_prices: [] },
    Script(fill_hash),
    tx,
  )
}

// Oracle freshness window tests (unit-style over helper)
// Ensures: timestamp must be >= (upper_bound - 60_000 ms).
test oracle_timestamp_fresh_and_stale_inclusive() {
  let hi = 1_000_000
  and {
    // Exactly at upper bound: valid
    order_lib.oracle_timestamp_valid(hi, True, hi, config.oracle_freshness_seconds),
    // Just above upper bound: valid
    order_lib.oracle_timestamp_valid(hi, True, hi + 1, config.oracle_freshness_seconds),
    // Just stale by 1 ms
    !order_lib.oracle_timestamp_valid(hi, True, hi - 60_001, config.oracle_freshness_seconds),
    // Far in the future is also valid
    order_lib.oracle_timestamp_valid(hi, True, hi + 10_000, config.oracle_freshness_seconds),
  }
}

test oracle_timestamp_freshness_only() {
  let hi = 2_000_000
  and {
    // Boundary of freshness window: valid
    order_lib.oracle_timestamp_valid(hi, False, hi - 60_000, config.oracle_freshness_seconds),
    // Equal to upper bound: valid
    order_lib.oracle_timestamp_valid(hi, False, hi, config.oracle_freshness_seconds),
    // Just above upper bound: valid
    order_lib.oracle_timestamp_valid(hi, False, hi + 1, config.oracle_freshness_seconds),
    // Below freshness window: invalid
    !order_lib.oracle_timestamp_valid(hi, False, hi - 60_001, config.oracle_freshness_seconds),
  }
}

// Cancel two orders in a single transaction
// Verifies stake-cancel validator path can handle multiple inputs/outputs,
// burns both NFTs, and checks owner signature aggregation.
test cancel_two_orders_batch() {
  // Build two order inputs (as if previously minted)
  let spent_ref = mk_output_reference(0)
  let in_order1 = Input {
    output_reference: OutputReference { transaction_id: mk_tx_hash(1), output_index: 0 },
    output: mk_two_way_output(spent_ref, 10, 1, 1_000_111, 0,
      Fixed { straight: Rational { numerator: 1, denominator: 2 }, reverse: None }),
  }
  let in_order2 = Input {
    output_reference: OutputReference { transaction_id: mk_tx_hash(2), output_index: 1 },
    output: mk_two_way_output(spent_ref, 11, 2, 2_000_222, 3_000_333,
      Fixed { straight: Rational { numerator: 2, denominator: 1 }, reverse: Some(Rational { numerator: 1, denominator: 2 }) }),
  }

  // Extract datums to compute expected cancel outputs
  expect InlineDatum(od1_raw) = in_order1.output.datum
  expect od1: OrderDatum = od1_raw
  expect InlineDatum(od2_raw) = in_order2.output.datum
  expect od2: OrderDatum = od2_raw

  // NFT names for both orders
  let nft1 = od1.nft_name
  let nft2 = od2.nft_name

  // Cancel outputs: return full value (minus NFT) to payment address with InlineDatum(nft_name)
  let out_cancel1 = Output {
    address: od1.payment_addr,
    value: assets.zero
      |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
      |> assets.add(od1.a.asset_class.policy_id, od1.a.asset_class.asset_name, od1.a.amount)
      |> assets.add(od1.b.asset_class.policy_id, od1.b.asset_class.asset_name, od1.b.amount),
    datum: InlineDatum(nft1),
    reference_script: None,
  }
  let out_cancel2 = Output {
    address: od2.payment_addr,
    value: assets.zero
      |> assets.add(ada_policy_id, ada_asset_name, config.min_deposit)
      |> assets.add(od2.a.asset_class.policy_id, od2.a.asset_class.asset_name, od2.a.amount)
      |> assets.add(od2.b.asset_class.policy_id, od2.b.asset_class.asset_name, od2.b.amount),
    datum: InlineDatum(nft2),
    reference_script: None,
  }

  // Owner signature aggregation: require one pubkey across both orders
  let sig_vkh = mk_hash(27, 42, #"")
  let od1_prime = OrderDatum {
    ..od1,
    owner: MultisigScript { signatures: [sig_vkh], scripts: [] },
  }
  let od2_prime = OrderDatum {
    ..od2,
    owner: MultisigScript { signatures: [sig_vkh], scripts: [] },
  }
  // Replace datums in inputs
  let in_order1_prime = Input { ..in_order1, output: Output { ..in_order1.output, datum: InlineDatum(od1_prime) } }
  let in_order2_prime = Input { ..in_order2, output: Output { ..in_order2.output, datum: InlineDatum(od2_prime) } }

  // Transaction with two inputs, two cancel outputs, burning both NFTs, config as ref input,
  // and extra_signatories including required key
  let tx = Transaction {
    ..transaction.placeholder,
    inputs: [in_order1_prime, in_order2_prime],
    reference_inputs: [config_ref_input],
    outputs: [out_cancel1, out_cancel2],
    mint:
      assets.zero
        |> assets.add(order_nft_policy_id, nft1, -1)
        |> assets.add(order_nft_policy_id, nft2, -1),
    extra_signatories: [sig_vkh],
  }

  order_val.order_validator_stake_cancel.withdraw(
    simple_nft,
    CancelOrder,
    Script(spend_hash),
    tx,
  )
}
