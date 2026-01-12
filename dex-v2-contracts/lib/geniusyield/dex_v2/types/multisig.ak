use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use geniusyield/dex_v2/address.{validate_credential_length}
use geniusyield/dex_v2/types/sorted_list.{
  SortedList, unsafe_from_list, verify_sorted,
}

/// Governs cancellation conditions for an order. Minting policy would ensure that when an order is created, this entries (`signatures` & `scripts`) are sorted.
pub type MultisigScript {
  /// List of signatures required to cancel the order.
  signatures: List<VerificationKeyHash>,
  /// List of stake validator scripts that must be satisfied to cancel the order.
  scripts: List<ScriptHash>,
}

/// Validate multisig script. Check if credentials are of correct length and that they are sorted.
pub fn validate_multisig_script(
  MultisigScript { signatures, scripts }: MultisigScript,
) -> Bool {
  and {
    list.all(signatures, validate_credential_length),
    list.all(scripts, validate_credential_length),
    verify_sorted(signatures, bytearray.compare),
    verify_sorted(scripts, bytearray.compare),
  }
}

// FIXME: I would likely need to remove the following types & functions.
pub type InternalMultisigScript {
  signatures: SortedList<VerificationKeyHash>,
  scripts: SortedList<ScriptHash>,
}

pub fn unsafe_cast_multisig_script(
  MultisigScript { signatures, scripts }: MultisigScript,
) -> InternalMultisigScript {
  InternalMultisigScript {
    signatures: unsafe_from_list(signatures),
    scripts: unsafe_from_list(scripts),
  }
}
