// SPDX-License-Identifier: BSL-1.1
// Copyright              : Copyright (c) 2025 GYELD GmbH
// License                : Business Source License 1.1
// Licensed Work          : GeniusYield Onchain Scripts
// Change License         : GPL v2.0 or later (after Change Date)
// See LICENSE file for full terms

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

// Order Minting Policy
// NB: only logic required for minting included here for efficiency
validator order_validator_mint(ref_nft: AssetClass, spend_hash: ScriptHash) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    // tokens requested to be minted by the transaction
    let new_mint_tokens = self.mint |> assets.tokens(policy_id) |> dict.filter(fn(_, amt) { amt > 0 })

    when redeemer is {
      // if burning: ensure no mint happens
      None -> new_mint_tokens == dict.empty

      // NB: it is not possible that two NFTs go to the same output because NFT asset name is put into datum
      Some(OutputReferenceInt {
        output_ref: check_spent_oref, // UTXO to ensure uniqueness
        amount: order_count,          // number of tokens to be minted
      }) -> {
        let nft_set = populate_nft_set(check_spent_oref, order_count, dict.empty) // (unordered) set of all NFTs to identify orders
        expect InlineDatum(config_raw) = find_marked_ref_input(self.reference_inputs, ref_nft).output.datum // ensure config reference input
        expect config: ConfigurationDatum = config_raw

        // checks: uniqueness UTXO consumed && orders < 256 && NFTs match && `verify_outputs` && `maker_fee_paid`
        let maker_fee_total = order_count * config.maker_lovelace_flat_fee
        let maker_fee_paid =
          if maker_fee_total == 0 { True }

          else {
            // fee output "agreed" to be first output if output list is non-empty (UTXO indexer)
            expect Some(fee_output) = list.head(self.outputs)
            and {
              fee_output.address == config.fee_addr,
              fee_output.datum == InlineDatum(policy_id),
              assets.match( fee_output.value, assets.zero |> assets.add(assets.ada_policy_id, assets.ada_asset_name, maker_fee_total), >=)
            }
          }

        and {
          gy_transaction.utxo_consumed(self.inputs, check_spent_oref)?,

          (order_count > 0)?,
          (order_count < 256)?,

          (nft_set == new_mint_tokens)?,

          verify_outputs(
            self.outputs,
            new_mint_tokens,
            policy_id,
            config,
            spend_hash,
          )?,

          maker_fee_paid?,
        }
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
    expect InlineDatum(config_raw) =
      find_marked_ref_input(reference_inputs, ref_nft).output.datum
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
        fn(_cred, _amt, _) {
          True
        },
      )
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
