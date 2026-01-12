//// Transaction related utilities.

use aiken/collection/list
use cardano/assets
use cardano/transaction.{Input, OutputReference}
use geniusyield/dex_v2/option.{assert}
use geniusyield/dex_v2/types/assets.{AssetClass} as gy_assets

/// Check if the given output is consumed by this transaction.
pub fn utxo_consumed(
  inputs: List<Input>,
  output_reference: OutputReference,
) -> Bool {
  inputs |> list.any(fn(input) { input.output_reference == output_reference })
}

/// Find the marked reference input (identified by the provided NFT) from the given list of reference inputs.
pub fn find_marked_ref_input(
  reference_inputs: List<Input>,
  nft: AssetClass,
) -> Input {
  reference_inputs
    |> list.find(
        fn(input) {
          (
            input.output.value
              |> assets.quantity_of(nft.policy_id, nft.asset_name)
          ) == 1
        },
      )
    |> assert()
}
