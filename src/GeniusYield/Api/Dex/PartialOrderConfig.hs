{- |
Module      : GeniusYield.Api.Dex.PartialOrderConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Api.Dex.PartialOrderConfig (
  PocdException (..),
  partialOrderConfigAddr',
  fetchPartialOrderConfig,
) where

import Control.Monad.Reader (ask)
import Data.Text qualified as Txt
import GeniusYield.Api.Dex.Types
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports
import GeniusYield.Scripts.Dex.PartialOrderConfig (
  PartialOrderConfigInfoF (..),
  partialOrderConfigAddr,
 )
import GeniusYield.TxBuilder (
  GYTxQueryMonad (networkId, utxosAtAddressWithDatums),
  addressFromPlutus',
  throwAppError,
  utxoDatumPure',
 )
import GeniusYield.Types (
  GYAddress,
  GYAssetClass,
  GYTxOutRef,
  GYUTxO (utxoRef),
 )
import Network.HTTP.Types (status400)

newtype PocdException = PocdException GYAssetClass
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError PocdException where
  toApiError (PocdException nftToken) =
    GYApiError
      { gaeErrorCode = "PARTIAL_ORDER_CONFIG_NOT_FOUND",
        gaeHttpStatus = status400,
        gaeMsg = Txt.pack $ printf "Partial order config not found for NFT: %s" nftToken
      }

partialOrderConfigAddr' ∷ GYDexApiQueryMonad m a ⇒ GYAssetClass → m GYAddress
partialOrderConfigAddr' nftToken = do
  nid ← networkId
  a ← ask
  pure $ partialOrderConfigAddr a nid nftToken

fetchPartialOrderConfig ∷ GYDexApiQueryMonad m a ⇒ GYAssetClass → m (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
fetchPartialOrderConfig nftToken = do
  addr ← partialOrderConfigAddr' nftToken
  utxos ← utxosAtAddressWithDatums addr $ Just nftToken
  case utxos of
    [p@(utxo, Just _)] → do
      (_, _, d') ← utxoDatumPure' p
      feeAddr ← addressFromPlutus' $ pociFeeAddr d'
      pure (utxoRef utxo, feeAddr <$ d')
    _ → throwAppError $ PocdException nftToken
