use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential}
use cardano/transaction.{InlineDatum, Transaction}
use geniusyield/dex_v2/order.{
  populate_nft_set, traverse_cancel_inputs_outputs, traverse_fill_inputs_outputs,
  verify_outputs,
} as order_lib
use geniusyield/dex_v2/transaction.{find_marked_ref_input} as gy_transaction
use geniusyield/dex_v2/types/assets.{AssetClass} as gy_assets
use geniusyield/dex_v2/types/order.{
  CancelOrder, ConfigurationDatum, FillOrder, FillRedeemer, MintRedeemer,
  OrderDatum, OrderRedeemer, OutputReferenceInt,
}
use geniusyield/dex_v2/types/sorted_list

// TODO: To not allow orders which uses say cancel/fill script within their cancellation conditions?
/// Order minting policy script.
///
/// Note that we would like order creation to have minimal cost, in this regard, size of reference script should be small and thus it makes sense to split logic not required for order creation in a separate script.
validator order_validator_mint(ref_nft: AssetClass, spend_hash: ScriptHash) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let new_mint_tokens =
      self.mint
        |> assets.tokens(policy_id)
        |> dict.filter(fn(_, amt) { amt > 0 })
    trace @"new_mint_tokens": new_mint_tokens
    trace @"redeemer": redeemer
    trace @"reference_inputs.len": list.length(self.reference_inputs)
    when redeemer is {
      // In the case of only burning, check that no new mint has occurred.
      None -> new_mint_tokens == dict.empty
      // Case where we are minting new tokens. Could it be possible that two NFTs go to the same output? No, as NFT's asset name is put into datum.
      Some(OutputReferenceInt {
        output_ref: check_spent_oref,
        amount: order_count,
      }) -> {
        trace @"before_populate": check_spent_oref
        trace @"order_count": order_count
        let nft_set =
          populate_nft_set(check_spent_oref, order_count, dict.empty)
        trace @"nft_set": nft_set
        let ref_input = find_marked_ref_input(self.reference_inputs, ref_nft)
        expect InlineDatum(config_raw) = ref_input.output.datum
        expect config: ConfigurationDatum = config_raw
        let maker_fee_total = order_count * config.maker_lovelace_flat_fee
        trace @"mint.maker_fee_total": maker_fee_total
        let maker_fee_paid =
          if maker_fee_total == 0 {
            True
          } else {
            list.any(
              self.outputs,
              fn(out) {
                and {
                  out.address == config.fee_addr,
                  out.datum == InlineDatum(policy_id),
                  assets.match(
                    out.value,
                    assets.zero
                      |> assets.add(
                          assets.ada_policy_id,
                          assets.ada_asset_name,
                          maker_fee_total,
                        ),
                    >=,
                  ),
                }
              },
            )
          }
        trace @"mint.maker_fee_paid": maker_fee_paid
        let cond_utxo_consumed = gy_transaction.utxo_consumed(self.inputs, check_spent_oref)
        let _ =
          when cond_utxo_consumed is {
            False -> fail @"mint:utxo_consumed"
            True -> True
          }
        let cond_order_count_gt0 = order_count > 0
        let _ =
          when cond_order_count_gt0 is {
            False -> fail @"mint:order_count_gt0"
            True -> True
          }
        // We don't actually need to do this check as `bytearray.push` would error out o/w.
        let cond_order_count_lt256 = order_count < 256
        let _ =
          when cond_order_count_lt256 is {
            False -> fail @"mint:order_count_lt256"
            True -> True
          }
        // Verify that new NFT minted has exactly the name we expect with correct amount.
        let cond_nft_set_matches = nft_set == new_mint_tokens
        let _ =
          when cond_nft_set_matches is {
            False -> fail @"mint:nft_set"
            True -> True
          }
        let _ =
          when verify_outputs(
            self.outputs,
            new_mint_tokens,
            policy_id,
            config,
            spend_hash,
          ) is {
            False -> fail @"mint:verify_outputs"
            True -> True
          }
        let _ =
          when maker_fee_paid is {
            False -> fail @"mint:maker_fee_paid"
            True -> True
          }
        True
      }
    }
  }

  else(_) {
    fail
  }
}

validator order_validator_spend(
  fill_credential: Credential,
  cancel_credential: Credential,
) {
  spend(
    _datum: Option<Data<OrderDatum>>,
    redeemer: OrderRedeemer,
    _utxo,
    Transaction { withdrawals, .. }: Transaction,
  ) {
    when redeemer is {
      CancelOrder -> pairs.has_key(withdrawals, cancel_credential)
      FillOrder -> pairs.has_key(withdrawals, fill_credential)
    }
  }

  else(_) {
    fail
  }
}

validator order_validator_stake_fill(ref_nft: AssetClass) {
  withdraw(
    redeemer: FillRedeemer,
    _account,
    Transaction {
      inputs,
      outputs,
      reference_inputs,
      withdrawals,
      mint,
      validity_range,
      ..
    }: Transaction,
  ) {
    trace @"cancel.ref_inputs": reference_inputs
    expect InlineDatum(config_raw) =
      find_marked_ref_input(reference_inputs, ref_nft).output.datum
    trace @"cancel.config_raw": config_raw
    expect config: ConfigurationDatum = config_raw
    let cancel_credential = Script(config.cancel_hash)
    let oracle_prices_dict =
      list.foldl(
        redeemer.oracle_prices,
        dict.empty,
        fn(entry, acc) {
          let (asset_name, details) = entry
          dict.insert(acc, asset_name, details)
        },
      )
    and {
      // Check that cancel is not being performed in same transaction.
      list.all(withdrawals, fn(Pair(cred, _)) { !(cred == cancel_credential) })?,
      traverse_fill_inputs_outputs(
        inputs,
        outputs,
        config.nft_policy_id,
        config,
        assets.tokens(mint, config.nft_policy_id)
          |> dict.filter(fn(_, amt) { amt < 0 }),
        dict.empty,
        assets.zero,
        assets.zero,
        False,
        0,
        config.fee_addr,
        reference_inputs,
        validity_range,
        oracle_prices_dict,
      )?,
    }
  }

  publish(
    _redeemer,
    certificate: Certificate,
    _tx: Transaction,
  ) {
    when certificate is {
      RegisterCredential { .. } -> True
      _ -> False
    }
  }

  else(_) {
    fail
  }
}

validator order_validator_stake_cancel(ref_nft: AssetClass) {
  withdraw(
    _redeemer,
    _account,
    Transaction {
      inputs,
      outputs,
      reference_inputs,
      withdrawals,
      mint,
      extra_signatories,
      ..
    }: Transaction,
  ) {
    expect InlineDatum(config_raw) =
      find_marked_ref_input(reference_inputs, ref_nft).output.datum
    expect config: ConfigurationDatum = config_raw
    let fill_credential = Script(config.fill_hash)
    let fill_wdrl = pairs.get_first(withdrawals, fill_credential)
    let no_fill =
      pairs.foldl(
        withdrawals,
        True,
        fn(cred, amt, ok) {
          ok && (if cred == fill_credential {
            amt == 0
          } else {
            True
          })
        },
      )
    expect _ =
      pairs.foldl(
        withdrawals,
        True,
        fn(cred, amt, _) {
          trace @"cancel.wdrl_entry": Pair(cred, amt)
          True
        },
      )
    trace @"cancel.withdrawals_len": list.length(withdrawals)
    trace @"cancel.fill_wdrl": fill_wdrl
    let cancel_ok =
      traverse_cancel_inputs_outputs(
        inputs,
        outputs,
        config.nft_policy_id,
        extra_signatories,
        mint
          |> assets.tokens(config.nft_policy_id)
          |> dict.filter(fn(_, amt) { amt < 0 }),
        withdrawals,
        sorted_list.empty,
        sorted_list.empty,
        dict.empty,
      )
    trace @"cancel.no_fill": no_fill
    trace @"cancel.ok": cancel_ok
    and { no_fill, cancel_ok }
  }

  publish(
    _redeemer,
    certificate: Certificate,
    _tx: Transaction,
  ) {
    when certificate is {
      RegisterCredential { .. } -> True
      _ -> False
    }
  }

  else(_) {
    fail
  }
}
