{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.Crypto (
  SignedMessage (..),
  Signature,
  PubKey,
  PaymentPubKey,
  ScriptHash,
) where

import GHC.Generics
import GeniusYield.OnChain.Core.Common.LedgerExports.Common
import PlutusTx qualified

type Signature = BuiltinByteString

type PubKey = LedgerBytes

type PaymentPubKey = PubKey

data SignedMessage a = SignedMessage
  { -- | The cryptographic signature.
    smSignature :: Signature,
    -- | The hash of the message.
    smMessageHash :: DatumHash
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''SignedMessage
