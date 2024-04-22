{- |
Module      : GeniusYield.Api.Dex.PartialOrderConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Api.Dex.PartialOrderConfig (
  PORef (..),
  SomePORef (..),
  withSomePORef,
  PORefs (..),
  PocdException (..),
  fetchPartialOrderConfig,
  unsafeFetchPartialOrderConfig,
  fetchPartialOrderConfig',
  unsafeFetchPartialOrderConfig',
  RefPocd (..),
  SomeRefPocd (..),
  withSomeRefPocd,
  RefPocds,
  selectV1RefPocd,
  selectV1_1RefPocd,
  selectRefPocd,
  selectRefPocd',
  selectPor,
  selectPor',
  fetchPartialOrderConfigs,
) where

import Control.Monad.Reader (ask)
import Data.Strict.Tuple (Pair (..))
import Data.Text qualified as Txt
import GeniusYield.Api.Dex.Types
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports
import GeniusYield.Scripts.Dex.PartialOrderConfig (
  HasPartialOrderConfigAddr (getPartialOrderConfigAddr),
  PartialOrderConfigInfoF (..),
 )
import GeniusYield.Scripts.Dex.Version
import GeniusYield.TxBuilder (
  GYTxQueryMonad (utxosAtAddressWithDatums),
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

data PORef (v ∷ POCVersion) = PORef
  { -- | The reference NFT.
    porRefNft ∷ !GYAssetClass,
    -- | The location of the reference NFT minting policy reference script.
    porMintRef ∷ !GYTxOutRef,
    -- | The location of the validator reference script.
    porValRef ∷ !GYTxOutRef
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SomePORef = ∀ v. SingPOCVersionI v ⇒ SomePORef (PORef v)

withSomePORef ∷ SomePORef → (∀ v. SingPOCVersionI v ⇒ PORef v → r) → r
withSomePORef (SomePORef por) f = f por

data PORefs = PORefs
  { -- | For the V1 version of partial order family of contract.
    porV1 ∷ !(PORef 'POCVersion1),
    -- | For the V1_1 version of partial order family of contract.
    porV1_1 ∷ !(PORef 'POCVersion1_1)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

newtype RefPocd (v ∷ POCVersion) = RefPocd (Pair GYTxOutRef (PartialOrderConfigInfoF GYAddress))

data SomeRefPocd = ∀ v. SingPOCVersionI v ⇒ SomeRefPocd (RefPocd v)

withSomeRefPocd ∷ SomeRefPocd → (∀ v. SingPOCVersionI v ⇒ RefPocd v → r) → r
withSomeRefPocd (SomeRefPocd por) f = f por

newtype RefPocds = RefPocds (Pair (RefPocd 'POCVersion1) (RefPocd 'POCVersion1_1))

selectV1RefPocd ∷ RefPocds → RefPocd 'POCVersion1
selectV1RefPocd (RefPocds (p :!: _)) = p

selectV1_1RefPocd ∷ RefPocds → RefPocd 'POCVersion1_1
selectV1_1RefPocd (RefPocds (_ :!: p)) = p

selectRefPocd ∷ RefPocds → POCVersion → SomeRefPocd
selectRefPocd refPocds pocVersion = withSomeSingPOCVersion (toSingPOCVersion pocVersion) (\(_ ∷ SingPOCVersion v) → SomeRefPocd (selectRefPocd' @v refPocds))

selectRefPocd' ∷ ∀ v. SingPOCVersionI v ⇒ RefPocds → RefPocd v
selectRefPocd' refPocds = case (singPOCVersion @v) of
  SingPOCVersion1 → selectV1RefPocd refPocds
  SingPOCVersion1_1 → selectV1_1RefPocd refPocds

selectPor ∷ PORefs → POCVersion → SomePORef
selectPor pors pocVersion = withSomeSingPOCVersion (toSingPOCVersion pocVersion) (\(_ ∷ SingPOCVersion v) → SomePORef (selectPor' @v pors))

selectPor' ∷ ∀ v. SingPOCVersionI v ⇒ PORefs → PORef v
selectPor' PORefs {..} = case (singPOCVersion @v) of
  SingPOCVersion1 → porV1
  SingPOCVersion1_1 → porV1_1

fetchPartialOrderConfig ∷ GYDexApiQueryMonad m a ⇒ POCVersion → PORefs → m SomeRefPocd
fetchPartialOrderConfig pocVersion pors =
  let SomePORef PORef {..} = selectPor pors pocVersion
   in unsafeFetchPartialOrderConfig pocVersion porRefNft

-- | Unsafe as it takes NFT's asset class where this NFT might not belong to the given version.
unsafeFetchPartialOrderConfig ∷ GYDexApiQueryMonad m a ⇒ POCVersion → GYAssetClass → m SomeRefPocd
unsafeFetchPartialOrderConfig pocVersion nftToken =
  withSomeSingPOCVersion (toSingPOCVersion pocVersion) $ \(_ ∷ SingPOCVersion v) → SomeRefPocd <$> unsafeFetchPartialOrderConfig' @v nftToken

-- | Unsafe as it takes NFT's asset class where this NFT might not belong to the given version.
unsafeFetchPartialOrderConfig' ∷ ∀ v m a. (GYDexApiQueryMonad m a, SingPOCVersionI v) ⇒ GYAssetClass → m (RefPocd v)
unsafeFetchPartialOrderConfig' nftToken = do
  a ← ask
  let pocVersion = fromSingPOCVersion $ singPOCVersion @v
      addr = getPartialOrderConfigAddr a pocVersion
  utxos ← utxosAtAddressWithDatums addr $ Just nftToken
  case utxos of
    [p@(utxo, Just _)] → do
      (_, _, d') ← utxoDatumPure' p
      feeAddr ← addressFromPlutus' $ pociFeeAddr d'
      pure $ RefPocd $ utxoRef utxo :!: feeAddr <$ d'
    _ → throwAppError $ PocdException nftToken

fetchPartialOrderConfig' ∷ ∀ v m a. (GYDexApiQueryMonad m a, SingPOCVersionI v) ⇒ PORefs → m (RefPocd v)
fetchPartialOrderConfig' pors = do
  let pocVersion = fromSingPOCVersion $ singPOCVersion @v
      SomePORef PORef {..} = selectPor pors pocVersion
  unsafeFetchPartialOrderConfig' @v porRefNft

fetchPartialOrderConfigs ∷ GYDexApiQueryMonad m a ⇒ PORefs → m RefPocds
fetchPartialOrderConfigs pors = do
  refPocd1 ← fetchPartialOrderConfig' @'POCVersion1 pors
  refPocd1_1 ← fetchPartialOrderConfig' @'POCVersion1_1 pors
  pure $ RefPocds $ refPocd1 :!: refPocd1_1
