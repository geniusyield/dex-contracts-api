use aiken/primitive/bytearray
use cardano/address.{
  Address, Credential, Inline, Pointer, Script, VerificationKey,
}

/// Validate whether the provided credential conform to ledger specification.
pub fn validate_credential(cred: Credential) -> Bool {
  let h =
    when cred is {
      VerificationKey(h) -> h
      Script(h) -> h
    }
  validate_credential_length(h)
}

/// Validate whether the provided credential length conform to ledger specification.
pub fn validate_credential_length(h: ByteArray) -> Bool {
  bytearray.length(h) == 28
}

/// Validate whether the provided address conform to ledger specification.
pub fn validate_address(
  Address { payment_credential, stake_credential },
) -> Bool {
  and {
    validate_credential(payment_credential),
    when stake_credential is {
      None -> True
      Some(rstake_credential) ->
        when rstake_credential is {
          Inline(sc) -> validate_credential(sc)
          // FIXME: To allow pointer?
          Pointer { .. } -> False
        }
    },
  }
}
