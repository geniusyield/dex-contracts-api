{-# LANGUAGE LambdaCase #-}

{- |
Module      : GeniusYield.Api.Dex.PartialOrder
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Api.Dex.PartialOrder (
  PORefs (..),
  PORef (..),

  -- * PartialOrderInfo
  PartialOrderInfo (..),
  POIContainedFee (..),
  partialOrderInfoToIn,
  partialOrderInfoToPayment,
  partialOrderInfoToPartialOrderDatum,
  poiGetContainedFeeValue,
  expectedPaymentWithDeposit,

  -- * Redeemer & Datum
  PartialOrderAction (..),
  PartialOrderDatum (..),

  -- * Partial order NFT policy
  partialOrderNftPolicy,
  partialOrderNftPolicyId,

  -- * Queries
  partialOrders,
  partialOrdersHavingAsset,
  orderByNft,
  getPartialOrderInfo,
  getPartialOrdersInfos,
  getPartialOrdersInfos',

  -- * Tx constructors
  placePartialOrder,
  placePartialOrder',
  completelyFillPartialOrder,
  partiallyFillPartialOrder,
  fillPartialOrder,
  fillPartialOrder',
  fillMultiplePartialOrders,
  completelyFillMultiplePartialOrders,
  fillMultiplePartialOrders',
  cancelPartialOrder,
  cancelMultiplePartialOrders,
  cancelMultiplePartialOrders',

  -- * Utilities
  partialOrderAddr,
  partialOrderPrice,
  partialOrderPrice',
  roundFunctionForPOCVersion,
  getVersionsInOrders,
  preferentiallySelectLatestVersion,
  preferentiallySelectLatestPocd,
) where

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.Reader (ask)
import Data.Bifunctor (Bifunctor)
import Data.Foldable (foldlM)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Strict.Tuple (Pair (..), (:!:))
import Data.Swagger qualified as Swagger
import Data.Text qualified as Text
import GeniusYield.Api.Dex.PartialOrderConfig (PORef (..), PORefs (..), RefPocd (..), RefPocds, SomePORef (..), SomeRefPocd (..), fetchPartialOrderConfig, fetchPartialOrderConfigs, selectPor, selectRefPocd, selectV1RefPocd, selectV1_1RefPocd, withSomePORef)
import GeniusYield.Api.Dex.Types (GYDexApiMonad, GYDexApiQueryMonad, HasDexScripts)
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Scripts.Dex.Nft (gyExpectedTokenName, mkNftRedeemer)
import GeniusYield.Scripts.Dex.PartialOrder (PartialOrderAction (..), PartialOrderContainedFee (..), PartialOrderDatum (..), PartialOrderFeeOutput (..), partialOrderValidator)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfo, PartialOrderConfigInfoF (..))
import GeniusYield.Scripts.Dex.PartialOrderNft (partialOrderNftMintingPolicy)
import GeniusYield.Scripts.Dex.Version
import GeniusYield.TxBuilder
import GeniusYield.Types
import Network.HTTP.Types.Status
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Ratio qualified as PlutusTx

data PodException
  = PodNftNotAvailable
  | PodNonPositiveAmount !Integer
  | PodNonPositivePrice !GYRational
  | PodRequestedAmountGreaterOrEqualToOfferedAmount {podReqAmt ∷ !Natural, podOfferedAmount ∷ !Natural}
  | PodRequestedAmountGreaterThanOfferedAmount {podReqAmt ∷ !Natural, podOfferedAmount ∷ !Natural}
  | -- | Offered asset is same as asked asset.
    PodNonDifferentAssets !GYAssetClass
  | PodEndEarlierThanStart
      !GYTime
      -- ^ Start time.
      !GYTime
      -- ^ End time.
  | PodNegativeFrontendFee !GYValue
  | -- | We couldn't fetch information for some of the given `GYTxOutRef`s. Note that this does not relate to UTxO being spent as depending upon provider, we would fetch information for even those `GYTxOutRef` which have been spent.
    PodNotAllOrderRefsPresent
      !(Set.Set GYTxOutRef)
      -- ^ Missing output refs.
  | -- | Such an order does not belong to supported swap script credentials.
    PodOrderDoesntBelongToScript !GYTxOutRef
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError PodException where
  toApiError PodNftNotAvailable =
    GYApiError
      { gaeErrorCode = "NFT_NOT_AVAILABLE",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack "Nft Not Available"
      }
  toApiError (PodNonPositiveAmount amt) =
    GYApiError
      { gaeErrorCode = "NON_POSITIVE_AMOUNT",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ "Amount was :  " ++ show amt
      }
  toApiError (PodNonPositivePrice price) =
    GYApiError
      { gaeErrorCode = "NON_POSITIVE_PRICE",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ "Price was:  " ++ show price
      }
  toApiError (PodRequestedAmountGreaterOrEqualToOfferedAmount reqAmt offeredAmt) =
    GYApiError
      { gaeErrorCode = "REQUESTED_AMOUNT_GREATER_OR_EQUAL_TO_OFFERED_AMOUNT",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ printf "Requested Amount was: %s but offered amount is %s" (show reqAmt) (show offeredAmt)
      }
  toApiError (PodRequestedAmountGreaterThanOfferedAmount reqAmt offeredAmt) =
    GYApiError
      { gaeErrorCode = "REQUESTED_AMOUNT_GREATER_THAN_OFFERED_AMOUNT",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ printf "Requested Amount was: %s but offered amount is %s" (show reqAmt) (show offeredAmt)
      }
  toApiError (PodNonDifferentAssets offeredAsset) =
    GYApiError
      { gaeErrorCode = "NOT_DIFFERENT_OFFERED_ASKED_ASSET",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ "Offered asset is same as asked asset, which is: " ++ show offeredAsset
      }
  toApiError (PodEndEarlierThanStart startTime endTime) =
    GYApiError
      { gaeErrorCode = "END_EARLIER_THAN_START",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ "End time is earlier than start. Start time: " ++ show startTime ++ ", end time: " ++ show endTime
      }
  toApiError (PodNegativeFrontendFee fee) =
    GYApiError
      { gaeErrorCode = "NEGATIVE_FRONTEND_FEE",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ "Negative frontend fee: " ++ show fee
      }
  toApiError (PodNotAllOrderRefsPresent missingRefs) =
    GYApiError
      { gaeErrorCode = "MISSING_REFS",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ "Not all of given references are present, missing ones: " ++ show missingRefs
      }
  toApiError (PodOrderDoesntBelongToScript ref) =
    GYApiError
      { gaeErrorCode = "ORDER_DOESNT_BELONG_TO_SCRIPT",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ "Order doesn't belong to supported swap script credentials: " ++ show ref
      }

-------------------------------------------------------------------------------
-- Order info
-------------------------------------------------------------------------------

data POIContainedFee = POIContainedFee
  { poifLovelaces ∷ !Natural,
    poifOfferedTokens ∷ !Natural,
    poifAskedTokens ∷ !Natural
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Swagger.ToSchema)

instance Semigroup POIContainedFee where
  (<>) a b =
    POIContainedFee
      { poifLovelaces = poifLovelaces a + poifLovelaces b,
        poifOfferedTokens = poifOfferedTokens a + poifOfferedTokens b,
        poifAskedTokens = poifAskedTokens a + poifAskedTokens b
      }

instance Monoid POIContainedFee where mempty = POIContainedFee 0 0 0

data PartialOrderInfo = PartialOrderInfo
  { -- | Reference to the partial order.
    poiRef ∷ !GYTxOutRef,
    -- | Public key hash of the owner.
    poiOwnerKey ∷ !GYPubKeyHash,
    -- | Address of the owner.
    poiOwnerAddr ∷ !GYAddress,
    -- | The asset being offered.
    poiOfferedAsset ∷ !GYAssetClass,
    -- | The number of units originally offered.
    poiOfferedOriginalAmount ∷ !Natural,
    -- | The number of units being offered.
    poiOfferedAmount ∷ !Natural,
    -- | The asset being asked for as payment.
    poiAskedAsset ∷ !GYAssetClass,
    -- | The price for one unit of the offered asset.
    poiPrice ∷ !GYRational,
    -- | Token name of the NFT identifying this partial order.
    poiNFT ∷ !GYTokenName,
    -- | The time when the order can earliest be filled (optional).
    poiStart ∷ !(Maybe GYTime),
    -- | The time when the order can latest be filled (optional).
    poiEnd ∷ !(Maybe GYTime),
    -- | The number of past partial fills.
    poiPartialFills ∷ !Natural,
    -- | Flat fee (in lovelace) paid by the taker.
    poiMakerLovelaceFlatFee ∷ !Natural,
    -- | Flat fee (in lovelace) paid by the taker.
    poiTakerLovelaceFlatFee ∷ !Natural,
    -- | Fee contained in the order.
    poiContainedFee ∷ !POIContainedFee,
    -- | Payment (in asked asset) contained in the order.
    poiContainedPayment ∷ !Natural,
    -- | Total value in the UTxO.
    poiUTxOValue ∷ !GYValue,
    -- | Address of the order UTxO.
    poiUTxOAddr ∷ !GYAddress,
    -- | Caching the CS to avoid recalculating for it.
    poiNFTCS ∷ !GYMintingPolicyId,
    -- | Version of the partial order.
    poiVersion ∷ !POCVersion
  }
  deriving stock (Show, Generic)
  deriving anyclass (Swagger.ToSchema)

poiContainedFeeToPlutus ∷ POIContainedFee → PartialOrderContainedFee
poiContainedFeeToPlutus POIContainedFee {..} =
  PartialOrderContainedFee
    { pocfLovelaces = fromIntegral poifLovelaces,
      pocfOfferedTokens = fromIntegral poifOfferedTokens,
      pocfAskedTokens = fromIntegral poifAskedTokens
    }

poiContainedFeeFromPlutus ∷ PartialOrderContainedFee → POIContainedFee
poiContainedFeeFromPlutus PartialOrderContainedFee {..} =
  POIContainedFee
    { poifLovelaces = fromIntegral pocfLovelaces,
      poifOfferedTokens = fromIntegral pocfOfferedTokens,
      poifAskedTokens = fromIntegral pocfAskedTokens
    }

partialOrderInfoToPartialOrderDatum ∷ PartialOrderInfo → PartialOrderDatum
partialOrderInfoToPartialOrderDatum PartialOrderInfo {..} =
  PartialOrderDatum
    { podOwnerKey = pubKeyHashToPlutus poiOwnerKey,
      podOwnerAddr = addressToPlutus poiOwnerAddr,
      podOfferedAsset = assetClassToPlutus poiOfferedAsset,
      podOfferedOriginalAmount = fromIntegral poiOfferedOriginalAmount,
      podOfferedAmount = fromIntegral poiOfferedAmount,
      podAskedAsset = assetClassToPlutus poiAskedAsset,
      podPrice = PlutusTx.fromGHC $ toRational poiPrice,
      podNFT = tokenNameToPlutus poiNFT,
      podStart = timeToPlutus <$> poiStart,
      podEnd = timeToPlutus <$> poiEnd,
      podPartialFills = fromIntegral poiPartialFills,
      podMakerLovelaceFlatFee = fromIntegral poiMakerLovelaceFlatFee,
      podTakerLovelaceFlatFee = toInteger poiTakerLovelaceFlatFee,
      podContainedFee = poiContainedFeeToPlutus poiContainedFee,
      podContainedPayment = toInteger poiContainedPayment
    }

poiGetContainedFeeValue ∷ PartialOrderInfo → GYValue
poiGetContainedFeeValue PartialOrderInfo {..} = poiContainedFeeToValue poiContainedFee poiOfferedAsset poiAskedAsset

poiContainedFeeToValue ∷ POIContainedFee → GYAssetClass → GYAssetClass → GYValue
poiContainedFeeToValue POIContainedFee {..} offAC askAC = valueSingleton GYLovelace (fromIntegral poifLovelaces) <> valueSingleton offAC (fromIntegral poifOfferedTokens) <> valueSingleton askAC (fromIntegral poifAskedTokens)

partialOrderInfoToIn
  ∷ HasDexScripts a
  ⇒ a
  → POCVersion
  → PORefs
  → PartialOrderInfo
  → PartialOrderAction
  → GYTxIn 'PlutusV2
partialOrderInfoToIn a pocVersion pors oi@PartialOrderInfo {..} oa =
  let SomePORef (PORef {..}) = selectPor pors pocVersion
   in GYTxIn
        { gyTxInTxOutRef = poiRef,
          gyTxInWitness =
            GYTxInWitnessScript
              (GYInReference porValRef $ validatorToScript $ partialOrderValidator a pocVersion porRefNft)
              (datumFromPlutusData $ partialOrderInfoToPartialOrderDatum oi)
              $ redeemerFromPlutusData oa
        }

partialOrderInfoToPayment ∷ PartialOrderInfo → GYValue → GYTxOut 'PlutusV2
partialOrderInfoToPayment oi v = mkGYTxOut (poiOwnerAddr oi) v (datumFromPlutusData $ txOutRefToPlutus $ poiRef oi)

partialOrderPrice ∷ PartialOrderInfo → Natural → GYValue
partialOrderPrice oi@PartialOrderInfo {poiAskedAsset} amt = valueSingleton poiAskedAsset $ fromIntegral $ partialOrderPrice' oi amt

roundFunctionForPOCVersion1_1 ∷ Integral a ⇒ Rational → a
roundFunctionForPOCVersion1_1 = floor

roundFunctionForPOCVersion ∷ Integral a ⇒ POCVersion → Rational → a
roundFunctionForPOCVersion = \case
  POCVersion1 → ceiling
  POCVersion1_1 → roundFunctionForPOCVersion1_1

partialOrderPrice' ∷ PartialOrderInfo → Natural → Natural
partialOrderPrice' PartialOrderInfo {poiPrice} amt = ceiling $ rationalToGHC poiPrice * toRational amt

{- | Note that at any moment, an order UTxO contains:-
        * An NFT.
        * Remaining offered tokens.
        * Payment for tokens consumed.
        * Initial deposit.
        * Collected fees.
-}
expectedPaymentWithDeposit ∷ PartialOrderInfo → Bool → GYValue
expectedPaymentWithDeposit poi@PartialOrderInfo {..} isCompleteFill =
  let toSubtract = valueSingleton (GYToken poiNFTCS poiNFT) 1 <> valueSingleton poiOfferedAsset (toInteger poiOfferedAmount) <> poiGetContainedFeeValue poi
      toAdd = if isCompleteFill then partialOrderPrice poi poiOfferedAmount else mempty
   in poiUTxOValue <> toAdd `valueMinus` toSubtract

-------------------------------------------------------------------------------
-- script address
-------------------------------------------------------------------------------

partialOrderAddr ∷ ∀ v m a. (GYDexApiQueryMonad m a, SingPOCVersionI v) ⇒ PORef v → m GYAddress
partialOrderAddr PORef {..} = do
  a ← ask
  scriptAddress $ partialOrderValidator a (fromSingPOCVersion $ singPOCVersion @v) porRefNft

partialOrderAddrTuple ∷ GYDexApiQueryMonad m a ⇒ PORefs → m (GYAddress :!: GYAddress)
partialOrderAddrTuple PORefs {..} = do
  addrV1 ← partialOrderAddr porV1
  addrV1_1 ← partialOrderAddr porV1_1
  pure $ addrV1 :!: addrV1_1

-------------------------------------------------------------------------------
-- partial order NFT policy
-------------------------------------------------------------------------------

partialOrderNftPolicy
  ∷ ∀ v m a
   . (GYDexApiQueryMonad m a, SingPOCVersionI v)
  ⇒ PORef v
  → m (GYMintingPolicy 'PlutusV2)
  -- ^ The minting policy of the partial order NFT.
partialOrderNftPolicy por = do
  a ← ask
  pure $ partialOrderNftPolicy' a por

partialOrderNftPolicy'
  ∷ ∀ v a
   . (SingPOCVersionI v, HasDexScripts a)
  ⇒ a
  → PORef v
  → GYMintingPolicy 'PlutusV2
  -- ^ The minting policy of the partial order NFT.
partialOrderNftPolicy' a PORef {..} = partialOrderNftMintingPolicy a (fromSingPOCVersion $ singPOCVersion @v) porRefNft

partialOrderNftPolicyId
  ∷ ∀ v m a
   . (GYDexApiQueryMonad m a, SingPOCVersionI v)
  ⇒ PORef v
  → m GYMintingPolicyId
  -- ^ The minting policy id of the partial order NFT.
partialOrderNftPolicyId por =
  mintingPolicyId <$> partialOrderNftPolicy por

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

partialOrders
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → m (Map.Map GYTxOutRef PartialOrderInfo)
partialOrders = flip partialOrdersHavingAsset Nothing

partialOrdersHavingAsset
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → Maybe GYAssetClass
  → m (Map.Map GYTxOutRef PartialOrderInfo)
partialOrdersHavingAsset pors hasAsset = do
  addrTuple ← partialOrderAddrTuple pors
  let pV1 :!: pV1_1 = applyToBoth (fromJust . addressToPaymentCredential) addrTuple
  utxosWithDatumsV1 ← utxosAtPaymentCredentialWithDatums pV1 hasAsset
  -- TODO: Add support in Atlas to query multiple payment credentials in one go.
  utxosWithDatumsV1_1 ← utxosAtPaymentCredentialWithDatums pV1_1 hasAsset
  policyIdV1 ← partialOrderNftPolicyId (porV1 pors)
  policyIdV1_1 ← partialOrderNftPolicyId (porV1_1 pors)
  let datumsV1 = utxosDatumsPure utxosWithDatumsV1
      datumsV1_1 = utxosDatumsPure utxosWithDatumsV1_1
  m1 ←
    iwither
      ( \oref vod →
          either (const Nothing) Just
            <$> runExceptT (makePartialOrderInfo policyIdV1 oref vod POCVersion1)
      )
      datumsV1
  m1_1 ←
    iwither
      ( \oref vod →
          either (const Nothing) Just
            <$> runExceptT (makePartialOrderInfo policyIdV1_1 oref vod POCVersion1_1)
      )
      datumsV1_1
  pure $! m1 <> m1_1

orderByNft
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → GYAssetClass
  → m (Maybe PartialOrderInfo)
orderByNft por orderNft = do
  ois ← partialOrdersHavingAsset por (Just orderNft)
  case Map.elems ois of
    [oi] → pure $ Just oi
    _ → pure Nothing

getPartialOrderVersion ∷ GYDexApiQueryMonad m a ⇒ PORefs → (GYAddress :!: GYTxOutRef) → m POCVersion
getPartialOrderVersion pors outxo = do
  ps ← applyToBoth addressToPaymentCredential <$> partialOrderAddrTuple pors
  getPartialOrderVersion' ps outxo

getPartialOrderVersion' ∷ GYDexApiQueryMonad m a ⇒ (Maybe GYPaymentCredential :!: Maybe GYPaymentCredential) → (GYAddress :!: GYTxOutRef) → m POCVersion
getPartialOrderVersion' (p1 :!: p1_1) (addr :!: oref) = do
  let pc = addressToPaymentCredential addr
  if
      | p1 == pc → pure POCVersion1
      | p1_1 == pc → pure POCVersion1_1
      | otherwise → throwAppError $ PodOrderDoesntBelongToScript oref

getPartialOrderInfo
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → GYTxOutRef
  → m PartialOrderInfo
getPartialOrderInfo pors orderRef = do
  utxoWithDatum ← utxoAtTxOutRefWithDatum' orderRef
  let utxo = fst utxoWithDatum
  pocVersion ← getPartialOrderVersion pors (utxoAddress utxo :!: utxoRef utxo)
  vod ← utxoDatumPure' utxoWithDatum
  policyId ← withSomePORef (selectPor pors pocVersion) partialOrderNftPolicyId

  runExceptT (makePartialOrderInfo policyId orderRef vod pocVersion) >>= liftEither

getPartialOrdersInfos
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → [GYTxOutRef]
  → m (Map.Map GYTxOutRef PartialOrderInfo)
getPartialOrdersInfos pors orderRefs = do
  utxosWithDatums ← utxosAtTxOutRefsWithDatums orderRefs
  ps ← applyToBoth addressToPaymentCredential <$> partialOrderAddrTuple pors
  let vod = utxosDatumsPure utxosWithDatums
  when (Map.size vod /= length orderRefs) $ throwAppError $ PodNotAllOrderRefsPresent $ Set.fromList orderRefs `Set.difference` Map.keysSet vod
  let makePartialOrderInfo' oref v@(addr, _, _) = do
        pocVersion ← getPartialOrderVersion' ps (addr :!: oref)
        policyId ← withSomePORef (selectPor pors pocVersion) partialOrderNftPolicyId
        makePartialOrderInfo policyId oref v pocVersion
  runExceptT (Map.traverseWithKey makePartialOrderInfo' vod) >>= liftEither

getPartialOrdersInfos' ∷ GYDexApiQueryMonad m a ⇒ PORefs → [(GYTxOutRef, Natural)] → m [(PartialOrderInfo, Natural)]
getPartialOrdersInfos' por ordersWithTokenBuyAmount = do
  let ordersWithTokenBuyAmount' = Map.fromList ordersWithTokenBuyAmount
  orders ← getPartialOrdersInfos por $ Map.keys ordersWithTokenBuyAmount' -- @Map.keys@ instead of @fst <$> ordersWithTokenBuyAmount@ just to make sure we don't give in duplicates, though not strictly necessary.
  -- Even though we use `dropMissing`, `getPartialOrdersInfos` verify that all entries are present.
  pure $ Map.elems $ Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched (\_ poi amt → (poi, amt))) orders ordersWithTokenBuyAmount'

makePartialOrderInfo
  ∷ GYDexApiQueryMonad m a
  ⇒ GYMintingPolicyId
  → GYTxOutRef
  → (GYAddress, GYValue, PartialOrderDatum)
  → POCVersion
  → ExceptT GYTxMonadException m PartialOrderInfo
makePartialOrderInfo policyId orderRef (utxoAddr, v, PartialOrderDatum {..}) pocVersion = do
  addr ← addressFromPlutus' podOwnerAddr

  key ← pubKeyHashFromPlutus' podOwnerKey

  offeredAsset ← assetClassFromPlutus' podOfferedAsset
  nft ← tokenNameFromPlutus' podNFT
  askedAsset ← assetClassFromPlutus' podAskedAsset

  when (valueAssetClass v (GYToken policyId nft) /= 1) $
    throwAppError PodNftNotAvailable

  return
    PartialOrderInfo
      { poiRef = orderRef,
        poiOwnerKey = key,
        poiOwnerAddr = addr,
        poiOfferedAsset = offeredAsset,
        poiOfferedOriginalAmount = fromInteger podOfferedOriginalAmount,
        poiOfferedAmount = fromInteger podOfferedAmount,
        poiAskedAsset = askedAsset,
        poiPrice = rationalFromPlutus podPrice,
        poiNFT = nft,
        poiStart = timeFromPlutus <$> podStart,
        poiEnd = timeFromPlutus <$> podEnd,
        poiPartialFills = fromInteger podPartialFills,
        poiMakerLovelaceFlatFee = fromIntegral podMakerLovelaceFlatFee,
        poiTakerLovelaceFlatFee = fromInteger podTakerLovelaceFlatFee,
        poiContainedFee = poiContainedFeeFromPlutus podContainedFee,
        poiContainedPayment = fromInteger podContainedPayment,
        poiUTxOValue = v,
        poiUTxOAddr = utxoAddr,
        poiNFTCS = policyId,
        poiVersion = pocVersion
      }

-------------------------------------------------------------------------------
-- Tx constructors
-------------------------------------------------------------------------------

placePartialOrder
  ∷ GYDexApiMonad m a
  ⇒ PORefs
  → GYAddress
  -- ^ Order owner
  → (Natural, GYAssetClass)
  -- ^ Amount and asset to offer.
  → GYAssetClass
  -- ^ The asset being asked for as payment.
  → GYRational
  -- ^ The price for one unit of the offered asset.
  → Maybe GYTime
  -- ^ The earliest time when the order can be filled (optional).
  → Maybe GYTime
  -- ^ The latest time when the order can be filled (optional).
  → Maybe GYStakeCredential
  -- ^ Stake credential of user. We do not support pointer reference.
  → m (GYTxSkeleton 'PlutusV2)
placePartialOrder pors = placePartialOrderWithVersion pors defaultPOCVersion

placePartialOrderWithVersion
  ∷ GYDexApiMonad m a
  ⇒ PORefs
  → POCVersion
  → GYAddress
  -- ^ Order owner
  → (Natural, GYAssetClass)
  -- ^ Amount and asset to offer.
  → GYAssetClass
  -- ^ The asset being asked for as payment.
  → GYRational
  -- ^ The price for one unit of the offered asset.
  → Maybe GYTime
  -- ^ The earliest time when the order can be filled (optional).
  → Maybe GYTime
  -- ^ The latest time when the order can be filled (optional).
  → Maybe GYStakeCredential
  -- ^ Stake credential of user. We do not support pointer reference.
  → m (GYTxSkeleton 'PlutusV2)
placePartialOrderWithVersion pors pocVersion addr (offerAmt, offerAC) priceAC price start end stakeCred = do
  SomeRefPocd (RefPocd (cfgRef :!: pocd)) ← fetchPartialOrderConfig pocVersion pors
  placePartialOrderWithVersion' pors pocVersion addr (offerAmt, offerAC) priceAC price start end 0 0 stakeCred cfgRef pocd

placePartialOrder'
  ∷ (GYDexApiMonad m a, HasCallStack)
  ⇒ PORefs
  → GYAddress
  -- ^ Order owner
  → (Natural, GYAssetClass)
  -- ^ Amount and asset to offer.
  → GYAssetClass
  -- ^ The asset being asked for as payment.
  → GYRational
  -- ^ The price for one unit of the offered asset.
  → Maybe GYTime
  -- ^ The earliest time when the order can be filled (optional).
  → Maybe GYTime
  -- ^ The latest time when the order can be filled (optional).
  → Natural
  -- ^ Additional lovelace fee.
  → Natural
  -- ^ Additional fee in offered tokens.
  → Maybe GYStakeCredential
  -- ^ Stake credential of user. We do not support pointer reference.
  → GYTxOutRef
  → PartialOrderConfigInfoF GYAddress
  → m (GYTxSkeleton 'PlutusV2)
placePartialOrder' pors = placePartialOrderWithVersion' pors defaultPOCVersion

placePartialOrderWithVersion'
  ∷ (GYDexApiMonad m a, HasCallStack)
  ⇒ PORefs
  → POCVersion
  → GYAddress
  -- ^ Order owner
  → (Natural, GYAssetClass)
  -- ^ Amount and asset to offer.
  → GYAssetClass
  -- ^ The asset being asked for as payment.
  → GYRational
  -- ^ The price for one unit of the offered asset.
  → Maybe GYTime
  -- ^ The earliest time when the order can be filled (optional).
  → Maybe GYTime
  -- ^ The latest time when the order can be filled (optional).
  → Natural
  -- ^ Additional lovelace fee.
  → Natural
  -- ^ Additional fee in offered tokens.
  → Maybe GYStakeCredential
  -- ^ Stake credential of user. We do not support pointer reference.
  → GYTxOutRef
  → PartialOrderConfigInfoF GYAddress
  → m (GYTxSkeleton 'PlutusV2)
placePartialOrderWithVersion' pors pocVersion addr (offerAmt, offerAC) priceAC price start end addLov addOff stakeCred cfgRef pocd = do
  when (offerAmt == 0) $ throwAppError $ PodNonPositiveAmount $ toInteger offerAmt
  when (price <= 0) $ throwAppError $ PodNonPositivePrice price
  when (offerAC == priceAC) $ throwAppError $ PodNonDifferentAssets offerAC

  case (start, end) of
    (Just start', Just end') → when (end' < start') $ throwAppError $ PodEndEarlierThanStart start' end'
    _ → pure ()

  let por@(SomePORef PORef {..}) = selectPor pors pocVersion

  pkh ← addressToPubKeyHash' addr
  outAddr ← withSomePORef por partialOrderAddr
  nid ← networkId
  let outAddr' = addressFromCredential nid (addressToPaymentCredential outAddr & fromJust) stakeCred
  policy ← withSomePORef por partialOrderNftPolicy
  nftRef ← someUTxOWithoutRefScript

  let nftName = gyExpectedTokenName nftRef
      nftRedeemer = mkNftRedeemer $ Just nftRef
      nft = GYToken (mintingPolicyId policy) nftName
      nftInput =
        GYTxIn
          { gyTxInTxOutRef = nftRef,
            gyTxInWitness = GYTxInWitnessKey
          }
      nftV = valueSingleton nft 1
      offerAmt' = toInteger offerAmt
      makerFeeFlat = fromIntegral addLov + pociMakerFeeFlat pocd
      makerFeeOff = (+) (fromIntegral addOff) $ roundFunctionForPOCVersion pocVersion $ toRational offerAmt * rationalToGHC (pociMakerFeeRatio pocd)
      makerFee =
        valueFromLovelace makerFeeFlat
          <> valueSingleton offerAC makerFeeOff
      offerV =
        valueSingleton offerAC offerAmt'
          <> nftV
          <> valueFromLovelace (toInteger $ pociMinDeposit pocd)
          <> makerFee
      containedFee =
        PartialOrderContainedFee
          { pocfLovelaces = makerFeeFlat,
            pocfOfferedTokens = makerFeeOff,
            pocfAskedTokens = 0
          }
      od =
        PartialOrderDatum
          { podOwnerKey = pubKeyHashToPlutus pkh,
            podOwnerAddr = addressToPlutus addr,
            podOfferedAsset = assetClassToPlutus offerAC,
            podOfferedOriginalAmount = offerAmt',
            podOfferedAmount = offerAmt',
            podAskedAsset = assetClassToPlutus priceAC,
            podPrice = rationalToPlutus price,
            podNFT = tokenNameToPlutus nftName,
            podStart = timeToPlutus <$> start,
            podEnd = timeToPlutus <$> end,
            podPartialFills = 0,
            podMakerLovelaceFlatFee = makerFeeFlat,
            podTakerLovelaceFlatFee = pociTakerFee pocd,
            podContainedFee = containedFee,
            podContainedPayment = 0
          }

      o = mkGYTxOut outAddr' offerV (datumFromPlutusData od)

  return $
    mustHaveInput nftInput
      <> mustHaveOutput o
      <> mustMint (GYMintReference porMintRef $ mintingPolicyToScript policy) nftRedeemer nftName 1
      <> mustHaveRefInput cfgRef

-- | Fills an order. If the provided amount of offered tokens to buy is equal to the offered amount, then we completely fill the order. Otherwise, it gets partially filled.
fillPartialOrder
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → GYTxOutRef
  -- ^ The order reference.
  → Natural
  -- ^ The amount of offered tokens to buy.
  → Maybe SomeRefPocd
  → Natural
  -- ^ Additional taker fee in payment tokens.
  → m (GYTxSkeleton 'PlutusV2)
fillPartialOrder por orderRef amt mRefPocd addTakerFee = do
  oi ← getPartialOrderInfo por orderRef
  fillPartialOrder' por oi amt mRefPocd addTakerFee

{- | Fills an order. If the provided amount of offered tokens to buy is equal to the offered amount, then we completely fill the order. Otherwise, it gets partially filled.

   This differs from `fillPartialOrder` in that it takes fetched order information instead of it's reference.
-}
fillPartialOrder'
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → PartialOrderInfo
  -- ^ The order information.
  → Natural
  -- ^ The amount of offered tokens to buy.
  → Maybe SomeRefPocd
  → Natural
  -- ^ Additional taker fee in payment tokens.
  → m (GYTxSkeleton 'PlutusV2)
fillPartialOrder' por oi@PartialOrderInfo {poiOfferedAmount} amt mRefPocd addTakerFee = do
  if amt == poiOfferedAmount
    then mkSkeletonCompletelyFillPartialOrder por oi mRefPocd addTakerFee
    else mkSkeletonPartiallyFillPartialOrder por oi amt mRefPocd addTakerFee

-- | Completely fill a partially-fillable order.
completelyFillPartialOrder
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → GYTxOutRef
  -- ^ The order reference.
  → Maybe SomeRefPocd
  → Natural
  -- ^ Additional taker fee in payment tokens.
  → m (GYTxSkeleton 'PlutusV2)
completelyFillPartialOrder por orderRef mRefPocd addTakerFee = do
  oi ← getPartialOrderInfo por orderRef
  mkSkeletonCompletelyFillPartialOrder por oi mRefPocd addTakerFee

-- | Partially fill a partially-fillable order.
partiallyFillPartialOrder
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → GYTxOutRef
  -- ^ The order reference.
  → Natural
  -- ^ The amount of offered tokens to buy.
  → Maybe SomeRefPocd
  → Natural
  -- ^ Additional taker fee in payment tokens.
  → m (GYTxSkeleton 'PlutusV2)
partiallyFillPartialOrder pors orderRef amt mRefPocd addTakerFee = do
  oi ← getPartialOrderInfo pors orderRef

  mkSkeletonPartiallyFillPartialOrder pors oi amt mRefPocd addTakerFee

-- | Creates the complete fill skeleton of a partial order.
mkSkeletonCompletelyFillPartialOrder
  ∷ (HasCallStack, GYDexApiQueryMonad m a)
  ⇒ PORefs
  → PartialOrderInfo
  → Maybe SomeRefPocd
  → Natural
  → m (GYTxSkeleton 'PlutusV2)
mkSkeletonCompletelyFillPartialOrder pors oi@PartialOrderInfo {..} mRefPocd addTakerFee = do
  pocVersion ← getPartialOrderVersion pors (poiUTxOAddr :!: poiRef)
  let por@(SomePORef PORef {..}) = selectPor pors pocVersion
  cs ← validFillRangeConstraints poiStart poiEnd
  gycs ← ask
  script ← mintingPolicyToScript <$> withSomePORef por partialOrderNftPolicy
  SomeRefPocd (RefPocd (cfgRef :!: pocd)) ←
    case mRefPocd of
      Just refPocd → pure refPocd
      Nothing → fetchPartialOrderConfig pocVersion pors

  let containedFee = poiGetContainedFeeValue oi
      fee = containedFee <> valueFromLovelace (fromIntegral poiTakerLovelaceFlatFee) <> valueSingleton poiAskedAsset (fromIntegral addTakerFee) -- Note that SC is fine if @addTakerFee@ is not included.
      feeOutput
        | fee == mempty = mempty -- We do not require a fee output.
        | otherwise =
            mustHaveOutput $
              mkGYTxOut
                (pociFeeAddr pocd)
                fee
                ( datumFromPlutusData $
                    PartialOrderFeeOutput
                      { pofdMentionedFees = PlutusTx.singleton (txOutRefToPlutus poiRef) (valueToPlutus containedFee),
                        pofdReservedValue = mempty,
                        pofdSpentUTxORef = Nothing
                      }
                )
      expectedValueOut = expectedPaymentWithDeposit oi True

  return $
    mustHaveInput (partialOrderInfoToIn gycs pocVersion pors oi CompleteFill)
      <> mustHaveRefInput cfgRef
      <> mustHaveOutput (partialOrderInfoToPayment oi expectedValueOut)
      <> feeOutput
      <> mustMint (GYMintReference porMintRef script) nothingRedeemer poiNFT (-1)
      <> cs

-- | Creates the partial fill skeleton of a partial order.
mkSkeletonPartiallyFillPartialOrder
  ∷ (HasCallStack, GYDexApiQueryMonad m a)
  ⇒ PORefs
  → PartialOrderInfo
  → Natural
  -- ^ The amount of offered tokens to buy.
  → Maybe SomeRefPocd
  → Natural
  → m (GYTxSkeleton 'PlutusV2)
mkSkeletonPartiallyFillPartialOrder pors oi@PartialOrderInfo {..} amt mRefPocd addTakerFee = do
  pocVersion ← getPartialOrderVersion pors (poiUTxOAddr :!: poiRef)
  when (amt == 0) . throwAppError $ PodNonPositiveAmount $ toInteger amt
  when (amt >= poiOfferedAmount) . throwAppError $ PodRequestedAmountGreaterOrEqualToOfferedAmount amt poiOfferedAmount

  SomeRefPocd (RefPocd (cfgRef :!: _pocd)) ←
    case mRefPocd of
      Just refPocd → pure refPocd
      Nothing → fetchPartialOrderConfig pocVersion pors

  let price' = partialOrderPrice oi amt
      od =
        partialOrderInfoToPartialOrderDatum
          oi
            { poiOfferedAmount = poiOfferedAmount - amt,
              poiPartialFills = poiPartialFills + 1,
              poiContainedFee = poiContainedFee <> mempty {poifLovelaces = fromIntegral poiTakerLovelaceFlatFee, poifAskedTokens = addTakerFee},
              poiContainedPayment = poiContainedPayment + fromIntegral (valueAssetClass price' poiAskedAsset)
            }

      expectedValueOut = poiUTxOValue <> price' <> valueFromLovelace (fromIntegral poiTakerLovelaceFlatFee) <> valueSingleton poiAskedAsset (fromIntegral addTakerFee) `valueMinus` valueSingleton poiOfferedAsset (toInteger amt)
      o = mkGYTxOut poiUTxOAddr expectedValueOut (datumFromPlutusData od)

  cs ← validFillRangeConstraints poiStart poiEnd
  gycs ← ask

  return $
    mustHaveInput (partialOrderInfoToIn gycs pocVersion pors oi $ PartialFill $ toInteger amt)
      <> mustHaveOutput o
      <> cs
      <> mustHaveRefInput cfgRef

cancelPartialOrder
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → GYTxOutRef
  → m (GYTxSkeleton 'PlutusV2)
cancelPartialOrder por orderRef = cancelMultiplePartialOrders por (pure orderRef)

-- | Cancel multiple partial orders.
cancelMultiplePartialOrders
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [GYTxOutRef]
  → m (GYTxSkeleton 'PlutusV2)
cancelMultiplePartialOrders pors orderRefs = do
  ois ← Map.elems <$> getPartialOrdersInfos pors orderRefs
  cancelMultiplePartialOrders' pors ois

getVersionsInOrders ∷ [PartialOrderInfo] → Set POCVersion
getVersionsInOrders = foldl' (\acc (PartialOrderInfo {..}) → Set.insert poiVersion acc) Set.empty

addCfgRefInputs ∷ Set POCVersion → RefPocds → GYTxSkeleton 'PlutusV2
addCfgRefInputs versionsSet cfgRefs =
  let RefPocd (cfgRefV1 :!: _) = selectV1RefPocd cfgRefs
      RefPocd (cfgRefV1_1 :!: _) = selectV1_1RefPocd cfgRefs
   in ( if Set.member POCVersion1 versionsSet
          then mustHaveRefInput cfgRefV1
          else mempty
      )
        <> ( if Set.member POCVersion1_1 versionsSet
              then mustHaveRefInput cfgRefV1_1
              else mempty
           )

preferentiallySelectLatestVersion ∷ Set POCVersion → POCVersion
preferentiallySelectLatestVersion versionsSet = fromMaybe maxBound (Set.lookupMax versionsSet)

-- | If there is a version 1.1 order in the set, then preferentially select it's config reference datum. Idea behind this is that when orders we are interacting with are all of same version, then we select that version's config reference datum but if it's a mixed bag, we select for the latest version.
preferentiallySelectLatestPocd ∷ Set POCVersion → RefPocds → PartialOrderConfigInfo
preferentiallySelectLatestPocd versionsSet cfgRefs =
  let overallVersion = preferentiallySelectLatestVersion versionsSet
      SomeRefPocd (RefPocd (_ :!: pocd)) = selectRefPocd cfgRefs overallVersion
   in pocd

-- | Cancel multiple partial orders.
cancelMultiplePartialOrders'
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [PartialOrderInfo]
  → m (GYTxSkeleton 'PlutusV2)
cancelMultiplePartialOrders' pors ois = do
  gycs ← ask
  cfgRefs ← fetchPartialOrderConfigs pors
  let versionsSet = getVersionsInOrders ois
  let pocd = preferentiallySelectLatestPocd versionsSet cfgRefs

  let (!feeOutputMap, !totalRequiredFees, !accumulatedSkeleton) =
        foldl'
          ( \(!mapAcc, !feeAcc, !skelAcc) poi@PartialOrderInfo {..} →
              let por@(SomePORef PORef {..}) = selectPor pors poiVersion
                  skelAdd =
                    mustHaveInput (partialOrderInfoToIn gycs poiVersion pors poi PartialCancel)
                      <> mustHaveOutput (partialOrderInfoToPayment poi (expectedPaymentWithDeposit poi False))
                      <> mustBeSignedBy poiOwnerKey
                      <> mustMint (GYMintReference porMintRef (withSomePORef por (partialOrderNftPolicy' gycs) & mintingPolicyToScript)) nothingRedeemer poiNFT (-1)
               in if poiPartialFills == 0 || poiContainedFee == mempty
                    then (mapAcc, feeAcc, skelAcc <> skelAdd)
                    else
                      let reqContainedFee =
                            let POIContainedFee {..} = poiContainedFee
                                feeToRefund ∷ Natural = floor $ (poiOfferedAmount % poiOfferedOriginalAmount) * (poifOfferedTokens % 1)
                             in POIContainedFee {poifLovelaces = poifLovelaces, poifOfferedTokens = poifOfferedTokens - feeToRefund, poifAskedTokens = poifAskedTokens}
                          reqContainedFeeValue = poiContainedFeeToValue reqContainedFee poiOfferedAsset poiAskedAsset
                       in (PlutusTx.unionWith (<>) mapAcc (PlutusTx.singleton (txOutRefToPlutus poiRef) (valueToPlutus reqContainedFeeValue)), feeAcc <> reqContainedFeeValue, skelAcc <> skelAdd)
          )
          (PlutusTx.empty, mempty, mempty)
          ois
      feeOutput
        | totalRequiredFees == mempty = mempty
        | otherwise =
            mustHaveOutput $ mkGYTxOut (pociFeeAddr pocd) totalRequiredFees $ datumFromPlutusData $ PartialOrderFeeOutput feeOutputMap mempty Nothing
  pure $
    feeOutput
      <> accumulatedSkeleton
      <> addCfgRefInputs versionsSet cfgRefs

-- | Fills multiple orders. If the provided amount of offered tokens to buy in an order is equal to the offered amount, then we completely fill the order. Otherwise, it gets partially filled.
fillMultiplePartialOrders
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [(GYTxOutRef, Natural)]
  → Maybe RefPocds
  → m (GYTxSkeleton 'PlutusV2)
fillMultiplePartialOrders pors ordersWithTokenBuyAmount mRefPocd = do
  ordersWithTokenBuyAmount' ← getPartialOrdersInfos' pors ordersWithTokenBuyAmount
  fillMultiplePartialOrders' pors ordersWithTokenBuyAmount' mRefPocd mempty

-- | Completely fill multiple orders.
completelyFillMultiplePartialOrders
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [GYTxOutRef]
  → Maybe RefPocds
  → m (GYTxSkeleton 'PlutusV2)
completelyFillMultiplePartialOrders por ordersRefs mRefPocd = do
  orders ← getPartialOrdersInfos por ordersRefs
  fillMultiplePartialOrders' por (map (\o → (o, poiOfferedAmount o)) $ Map.elems orders) mRefPocd mempty

-- | Fills multiple orders. If the provided amount of offered tokens to buy in an order is equal to the offered amount, then we completely fill the order. Otherwise, it gets partially filled.
fillMultiplePartialOrders'
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [(PartialOrderInfo, Natural)]
  → Maybe RefPocds
  → GYValue
  -- ^ Additional taker fee.
  → m (GYTxSkeleton 'PlutusV2)
fillMultiplePartialOrders' pors orders mRefPocd addTakerFee = do
  cfgRefs ←
    case mRefPocd of
      Just refPocds → pure refPocds
      Nothing → fetchPartialOrderConfigs pors
  gycs ← ask
  let versionsSet = getVersionsInOrders $ map fst orders
      pocd = preferentiallySelectLatestPocd versionsSet cfgRefs
      cfgRefInputs = addCfgRefInputs versionsSet cfgRefs
      buildWithFeeOutput = do
        let (!feeOutputMap, !totalContainedFee, !maxTakerFee) =
              foldl'
                ( \(!mapAcc, !feeAcc, !prevMaxTakerFee) (PartialOrderInfo {..}, amtToFill) →
                    let curMaxTakerFee = max prevMaxTakerFee poiTakerLovelaceFlatFee
                     in if amtToFill == poiOfferedAmount
                          then
                            let orderContainedFee = poiContainedFeeToValue poiContainedFee poiOfferedAsset poiAskedAsset
                             in (PlutusTx.unionWith (<>) mapAcc (PlutusTx.singleton (txOutRefToPlutus poiRef) (valueToPlutus orderContainedFee)), feeAcc <> orderContainedFee, curMaxTakerFee)
                          else (mapAcc, feeAcc, curMaxTakerFee)
                )
                (PlutusTx.empty, mempty, 0)
                orders
            fee = totalContainedFee <> valueFromLovelace (fromIntegral maxTakerFee)
            feeOutput
              | fee == mempty = mempty
              | otherwise =
                  mustHaveOutput $ mkGYTxOut (pociFeeAddr pocd) (fee <> addTakerFee) $ datumFromPlutusData $ PartialOrderFeeOutput feeOutputMap mempty Nothing
        foldlM
          ( \(!prevSkel) (poi@PartialOrderInfo {..}, amt) → do
              commonCheck amt poiOfferedAmount
              cs ← validFillRangeConstraints poiStart poiEnd
              let por@(SomePORef PORef {..}) = selectPor pors poiVersion
                  skel =
                    if amt == poiOfferedAmount
                      then
                        let expectedValueOut = expectedPaymentWithDeposit poi True
                         in mustHaveInput (partialOrderInfoToIn gycs poiVersion pors poi CompleteFill)
                              <> mustHaveOutput (partialOrderInfoToPayment poi expectedValueOut)
                              <> mustMint (GYMintReference porMintRef (withSomePORef por (partialOrderNftPolicy' gycs) & mintingPolicyToScript)) nothingRedeemer poiNFT (-1)
                              <> cs
                      else
                        let price' = partialOrderPrice poi amt
                            od =
                              partialOrderInfoToPartialOrderDatum
                                poi
                                  { poiOfferedAmount = poiOfferedAmount - amt,
                                    poiPartialFills = poiPartialFills + 1,
                                    poiContainedPayment = poiContainedPayment + fromIntegral (valueAssetClass price' poiAskedAsset)
                                  }

                            expectedValueOut = poiUTxOValue <> price' `valueMinus` valueSingleton poiOfferedAsset (toInteger amt)
                            o = mkGYTxOut poiUTxOAddr expectedValueOut (datumFromPlutusData od)
                         in mustHaveInput (partialOrderInfoToIn gycs poiVersion pors poi $ PartialFill $ toInteger amt)
                              <> mustHaveOutput o
                              <> cs

              pure $! prevSkel <> skel
          )
          (cfgRefInputs <> feeOutput)
          orders

  let buildWithoutFeeOutput = do
        let maxTakerFee = foldl' (\prevMaxTakerFee (PartialOrderInfo {..}, _) → max prevMaxTakerFee poiTakerLovelaceFlatFee) 0 orders
        foldlM
          ( \(!prevSkel) (idx, (poi@PartialOrderInfo {..}, amt)) → do
              commonCheck amt poiOfferedAmount
              let price' = partialOrderPrice poi amt
                  tf = if idx == 1 then mempty {poifLovelaces = fromIntegral maxTakerFee} else mempty
                  od =
                    partialOrderInfoToPartialOrderDatum
                      poi
                        { poiOfferedAmount = poiOfferedAmount - amt,
                          poiPartialFills = poiPartialFills + 1,
                          poiContainedFee = poiContainedFee <> tf,
                          poiContainedPayment = poiContainedPayment + fromIntegral (valueAssetClass price' poiAskedAsset)
                        }

                  expectedValueOut = poiUTxOValue <> price' <> poiContainedFeeToValue tf poiOfferedAsset poiAskedAsset `valueMinus` valueSingleton poiOfferedAsset (toInteger amt)
                  o = mkGYTxOut poiUTxOAddr expectedValueOut (datumFromPlutusData od)

              cs ← validFillRangeConstraints poiStart poiEnd

              pure $!
                prevSkel
                  <> mustHaveInput (partialOrderInfoToIn gycs poiVersion pors poi $ PartialFill $ toInteger amt)
                  <> mustHaveOutput o
                  <> cs
          )
          cfgRefInputs
          (zip [(1 ∷ Natural) ..] orders)
  -- If all orders are of same version, we can keep earlier logic of not requiring fee output when all orders are filled partially.
  if (Set.size versionsSet > 1) || isJust (find (\(PartialOrderInfo {..}, amt) → amt == poiOfferedAmount) orders)
    then buildWithFeeOutput
    else buildWithoutFeeOutput
 where
  commonCheck amt poiOfferedAmount = do
    when (amt == 0) . throwAppError $ PodNonPositiveAmount $ toInteger amt
    when (amt > poiOfferedAmount) . throwAppError $ PodRequestedAmountGreaterThanOfferedAmount amt poiOfferedAmount

applyToBoth ∷ Bifunctor p ⇒ (c → d) → p c c → p d d
applyToBoth f = bimap f f

-- | Exceptions raised while (partially) filling (partial) orders.
data FillOrderException
  = -- | Attempt to (partially) fill an order too early.
    TooEarlyFill {foeStart ∷ !GYSlot, foeNow ∷ !GYSlot}
  | -- | Attempt to (partially) fill an order too late.
    TooLateFill {foeEnd ∷ !GYSlot, foeNow ∷ !GYSlot}
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError FillOrderException where
  toApiError (TooEarlyFill start now) =
    GYApiError
      { gaeErrorCode = "TOO_EARLY_FILL",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ printf "Order cannot be filled before slot %s\ncurrent slot: %s" start now
      }
  toApiError (TooLateFill end now) =
    GYApiError
      { gaeErrorCode = "TOO_LATE_FILL",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ printf "Order cannot be filled after slot %s\ncurrent slot: %s" end now
      }

validFillRangeConstraints ∷ ∀ m. GYTxQueryMonad m ⇒ Maybe GYTime → Maybe GYTime → m (GYTxSkeleton 'PlutusV2)
validFillRangeConstraints mstart mend = (<>) <$> startConstraint <*> endConstraint
 where
  startConstraint ∷ m (GYTxSkeleton 'PlutusV2)
  startConstraint = case mstart of
    Nothing → return mempty
    Just start → do
      now ← slotOfCurrentBlock
      startSlot ← enclosingSlotFromTime' start
      if now >= startSlot
        then return $ isInvalidBefore now
        else throwAppError $ TooEarlyFill {foeStart = startSlot, foeNow = now}

  endConstraint ∷ m (GYTxSkeleton 'PlutusV2)
  endConstraint = case mend of
    Nothing → return mempty
    Just end → do
      now ← slotOfCurrentBlock
      endSlot ← enclosingSlotFromTime' end
      if now <= endSlot
        then return $ isInvalidAfter $ min endSlot $ unsafeAdvanceSlot now 120
        else throwAppError $ TooLateFill {foeEnd = endSlot, foeNow = now}
