{- |
Module      : GeniusYield.Scripts.Dex.Nft
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Scripts.Dex.Nft (
  mkNftRedeemer,

  -- * shared functions
  expectedTokenName,
  gyExpectedTokenName,
) where

import Data.Maybe (fromJust)
import GeniusYield.Types
import PlutusLedgerApi.V1 qualified as Plutus (
  TokenName (..),
  TxId (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins qualified as Plutus (
  BuiltinByteString,
  consByteString,
  sha2_256,
 )

mkNftRedeemer ∷ Maybe GYTxOutRef → GYRedeemer
mkNftRedeemer = redeemerFromPlutusData . fmap txOutRefToPlutus

expectedTokenName ∷ Plutus.TxOutRef → Plutus.TokenName
expectedTokenName (Plutus.TxOutRef (Plutus.TxId tid) ix) = Plutus.TokenName s
 where
  s ∷ Plutus.BuiltinByteString
  s = Plutus.sha2_256 (Plutus.consByteString ix tid)

gyExpectedTokenName ∷ GYTxOutRef → GYTokenName
gyExpectedTokenName =
  fromJust
    . tokenNameFromPlutus
    . expectedTokenName
    . txOutRefToPlutus
