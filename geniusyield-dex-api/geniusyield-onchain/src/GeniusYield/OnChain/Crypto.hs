{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}

module GeniusYield.OnChain.Crypto (
  -- * Plutus types
  DatumHash (..),
  PubKey,
  Signature,
  TxInfo (..),
  UnsafeFromData (..),

  -- * Signed message
  SignedMessage,

  -- * Signature verification
  verifySignedMessageOnChain,
) where

import GeniusYield.OnChain.Core.Common.Crypto
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Contexts (findDatum)
import PlutusTx.Prelude

{-# INLINEABLE verifySignedMessageOnChain #-}

{- | Verifies a signed message.
This requires that the message is included as datum in the `TxInfo` argument and will result in an error if
the signature is invalid or if the datum with the correct hash (included in the `SignedMessage`) can not be found.
-}
verifySignedMessageOnChain
  :: forall a
   . UnsafeFromData a
  => TxInfo
  -- ^ The transaction info, which must contain the message as datum.
  -> PubKey
  -- ^ The public key of the message signer.
  -> SignedMessage a
  -- ^ The signed message.
  -> a
  -- ^ Returns the message.
verifySignedMessageOnChain info (LedgerBytes pk) SignedMessage {smSignature, smMessageHash}
  | not $ verifyEd25519Signature pk h smSignature = traceError "invalid signature"
  | otherwise = a
 where
  h :: BuiltinByteString
  DatumHash h = smMessageHash

  a :: a
  a = case findDatum smMessageHash info of
    Nothing -> traceError "datum not found"
    Just (Datum d) -> unsafeFromBuiltinData d
