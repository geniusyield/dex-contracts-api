module GeniusYield.OnChain.Core.Common.Utils (
  expectedTokenName,
  expectedTwoTokenName,
  expectedTwoTokenNameN,
) where

import GeniusYield.OnChain.Core.Common.LedgerExports.Common
import PlutusTx.Prelude (appendByteString, consByteString, emptyByteString, sha2_256, sliceByteString)

expectedTokenName :: TxOutRef -> TokenName
expectedTokenName (TxOutRef (TxId tid) ix) = TokenName s
 where
  -- TODO: this works if ix < 256; which is hopefully often & can be arranged.
  -- having a loop with modulo doesn't seem to work.
  s :: BuiltinByteString
  s = sha2_256 (consByteString ix tid)

-- TWO-specific: truncated sha256 (28 bytes) with a leading 1-byte counter (1 for our mint path)
expectedTwoTokenName :: TxOutRef -> TokenName
expectedTwoTokenName ref = expectedTwoTokenNameN ref 1

-- TWO-specific: same as expectedTwoTokenName but with an explicit 1-byte counter (1..255)
expectedTwoTokenNameN :: TxOutRef -> Integer -> TokenName
expectedTwoTokenNameN (TxOutRef (TxId tid) ix) n = TokenName s
 where
  base :: BuiltinByteString
  base = sha2_256 (consByteString ix tid)
  base28 :: BuiltinByteString
  base28 = sliceByteString 0 28 base
  prefix :: BuiltinByteString
  prefix = consByteString (fromInteger n) emptyByteString
  s :: BuiltinByteString
  s = appendByteString prefix base28
