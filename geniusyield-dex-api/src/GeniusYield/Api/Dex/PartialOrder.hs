{- |
Module      : GeniusYield.Api.Dex.PartialOrder
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Api.Dex.PartialOrder (
  PORefs (..),

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
  getPartialOrderInfo,
  getPartialOrdersInfos,

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
  cancelPartialOrder',
  cancelMultiplePartialOrders,

  -- * Utilities
  partialOrderAddr,
  partialOrderPrice,
  partialOrderPrice',
) where

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.Reader (ask)
import Data.Foldable (foldlM)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Swagger qualified as Swagger
import Data.Text qualified as Text
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.Api.Dex.Types (GYDexApiMonad, GYDexApiQueryMonad, HasDexScripts)
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Scripts.Dex.Nft (gyExpectedTokenName, mkNftRedeemer)
import GeniusYield.Scripts.Dex.PartialOrder (PartialOrderAction (..), PartialOrderContainedFee (..), PartialOrderDatum (..), PartialOrderFeeOutput (..), partialOrderValidator)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Scripts.Dex.PartialOrderNft (partialOrderNftMintingPolicy)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Network.HTTP.Types.Status
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Ratio qualified as PlutusTx

data PORefs = PORefs
  { -- | The reference NFT.
    porRefNft ∷ !GYAssetClass,
    -- | The location of the reference NFT minting policy reference script.
    porMintRef ∷ !GYTxOutRef,
    -- | The location of the validator reference script.
    porValRef ∷ !GYTxOutRef
  }
  deriving (Show)

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

-------------------------------------------------------------------------------
-- Order info
-------------------------------------------------------------------------------

data POIContainedFee = POIContainedFee
  { poifLovelaces ∷ !Natural,
    poifOfferedTokens ∷ !Natural,
    poifAskedTokens ∷ !Natural
  }
  deriving stock (Eq, Show, Generic)
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
    poiNFTCS ∷ !GYMintingPolicyId
  }
  deriving stock (Eq, Show, Generic)
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
  → PORefs
  → PartialOrderInfo
  → PartialOrderAction
  → GYTxIn 'PlutusV2
partialOrderInfoToIn a PORefs {..} oi@PartialOrderInfo {..} oa =
  GYTxIn
    { gyTxInTxOutRef = poiRef,
      gyTxInWitness =
        GYTxInWitnessScript
          (GYInReference porValRef $ validatorToScript $ partialOrderValidator a porRefNft)
          (datumFromPlutusData $ partialOrderInfoToPartialOrderDatum oi)
          $ redeemerFromPlutusData oa
    }

partialOrderInfoToPayment ∷ PartialOrderInfo → GYValue → GYTxOut 'PlutusV2
partialOrderInfoToPayment oi v = mkGYTxOut (poiOwnerAddr oi) v (datumFromPlutusData $ txOutRefToPlutus $ poiRef oi)

partialOrderPrice ∷ PartialOrderInfo → Natural → GYValue
partialOrderPrice oi@PartialOrderInfo {poiAskedAsset} amt = valueSingleton poiAskedAsset $ fromIntegral $ partialOrderPrice' oi amt

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

partialOrderAddr ∷ GYDexApiQueryMonad m a ⇒ PORefs → m GYAddress
partialOrderAddr PORefs {..} = do
  a ← ask
  scriptAddress $ partialOrderValidator a porRefNft

-------------------------------------------------------------------------------
-- partial order NFT policy
-------------------------------------------------------------------------------

partialOrderNftPolicy
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → m (GYMintingPolicy 'PlutusV2)
  -- ^ The minting policy of the partial order NFT.
partialOrderNftPolicy PORefs {..} = do
  a ← ask
  pure $ partialOrderNftMintingPolicy a porRefNft

partialOrderNftPolicyId
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
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
partialOrders por = do
  addr ← partialOrderAddr por
  let paymentCred = addressToPaymentCredential addr & fromJust
  utxosWithDatums ← utxosAtPaymentCredentialWithDatums paymentCred
  policyId ← partialOrderNftPolicyId por
  let datums = utxosDatumsPure utxosWithDatums
  iwither
    ( \oref vod →
        either (const Nothing) Just
          <$> runExceptT (makePartialOrderInfo policyId oref vod)
    )
    datums

getPartialOrderInfo
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → GYTxOutRef
  → m PartialOrderInfo
getPartialOrderInfo por orderRef = do
  utxoWithDatum ← utxoAtTxOutRefWithDatum' orderRef
  vod ← utxoDatumPure' utxoWithDatum
  policyId ← partialOrderNftPolicyId por

  runExceptT (makePartialOrderInfo policyId orderRef vod) >>= liftEither

getPartialOrdersInfos
  ∷ GYDexApiQueryMonad m a
  ⇒ PORefs
  → [GYTxOutRef]
  → m (Map.Map GYTxOutRef PartialOrderInfo)
getPartialOrdersInfos por orderRefs = do
  utxosWithDatums ← utxosAtTxOutRefsWithDatums orderRefs
  let vod = utxosDatumsPure utxosWithDatums
  when (Map.size vod /= length orderRefs) $ throwAppError $ PodNotAllOrderRefsPresent $ Set.fromList orderRefs `Set.difference` Map.keysSet vod
  policyId ← partialOrderNftPolicyId por
  runExceptT (Map.traverseWithKey (makePartialOrderInfo policyId) vod) >>= liftEither

makePartialOrderInfo
  ∷ GYDexApiQueryMonad m a
  ⇒ GYMintingPolicyId
  → GYTxOutRef
  → (GYAddress, GYValue, PartialOrderDatum)
  → ExceptT GYTxMonadException m PartialOrderInfo
makePartialOrderInfo policyId orderRef (utxoAddr, v, PartialOrderDatum {..}) = do
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
        poiNFTCS = policyId
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
placePartialOrder por@PORefs {..} addr (offerAmt, offerAC) priceAC price start end stakeCred = do
  (cfgRef, pocd) ← fetchPartialOrderConfig porRefNft
  placePartialOrder' por addr (offerAmt, offerAC) priceAC price start end 0 0 stakeCred cfgRef pocd

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
placePartialOrder' por@PORefs {..} addr (offerAmt, offerAC) priceAC price start end addLov addOff stakeCred cfgRef pocd = do
  when (offerAmt == 0) $ throwAppError $ PodNonPositiveAmount $ toInteger offerAmt
  when (price <= 0) $ throwAppError $ PodNonPositivePrice price
  when (offerAC == priceAC) $ throwAppError $ PodNonDifferentAssets offerAC

  case (start, end) of
    (Just start', Just end') → when (end' < start') $ throwAppError $ PodEndEarlierThanStart start' end'
    _ → pure ()

  pkh ← addressToPubKeyHash' addr
  outAddr ← partialOrderAddr por
  nid ← networkId
  let outAddr' = addressFromCredential nid (addressToPaymentCredential outAddr & fromJust) stakeCred
  policy ← partialOrderNftPolicy por
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
      makerFeeOff = (+) (fromIntegral addOff) $ ceiling $ toRational offerAmt * rationalToGHC (pociMakerFeeRatio pocd)
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
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
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
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
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
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
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
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
  → Natural
  -- ^ Additional taker fee in payment tokens.
  → m (GYTxSkeleton 'PlutusV2)
partiallyFillPartialOrder por orderRef amt mRefPocd addTakerFee = do
  oi ← getPartialOrderInfo por orderRef

  mkSkeletonPartiallyFillPartialOrder por oi amt mRefPocd addTakerFee

-- | Creates the complete fill skeleton of a partial order.
mkSkeletonCompletelyFillPartialOrder
  ∷ (HasCallStack, GYDexApiQueryMonad m a)
  ⇒ PORefs
  → PartialOrderInfo
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
  → Natural
  → m (GYTxSkeleton 'PlutusV2)
mkSkeletonCompletelyFillPartialOrder por@PORefs {..} oi@PartialOrderInfo {..} mRefPocd addTakerFee = do
  cs ← validFillRangeConstraints poiStart poiEnd
  gycs ← ask
  script ← mintingPolicyToScript <$> partialOrderNftPolicy por
  (cfgRef, pocd) ←
    case mRefPocd of
      Just (cfgRef', pocd') → pure (cfgRef', pocd')
      Nothing → fetchPartialOrderConfig porRefNft

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
    mustHaveInput (partialOrderInfoToIn gycs por oi CompleteFill)
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
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
  → Natural
  → m (GYTxSkeleton 'PlutusV2)
mkSkeletonPartiallyFillPartialOrder por@PORefs {..} oi@PartialOrderInfo {..} amt mRefPocd addTakerFee = do
  when (amt == 0) . throwAppError $ PodNonPositiveAmount $ toInteger amt
  when (amt >= poiOfferedAmount) . throwAppError $ PodRequestedAmountGreaterOrEqualToOfferedAmount amt poiOfferedAmount

  (cfgRef, _pocd) ←
    case mRefPocd of
      Just (cfgRef', pocd') → pure (cfgRef', pocd')
      Nothing → fetchPartialOrderConfig porRefNft

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
    mustHaveInput (partialOrderInfoToIn gycs por oi $ PartialFill $ toInteger amt)
      <> mustHaveOutput o
      <> cs
      <> mustHaveRefInput cfgRef

cancelPartialOrder
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → GYTxOutRef
  -- ^ The order reference.
  → m (GYTxSkeleton 'PlutusV2)
cancelPartialOrder por orderRef = do
  poi ← getPartialOrderInfo por orderRef
  cancelMultiplePartialOrders por [poi]

cancelPartialOrder'
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → PartialOrderInfo
  -- ^ The order information.
  → m (GYTxSkeleton 'PlutusV2)
cancelPartialOrder' por poi = cancelMultiplePartialOrders por [poi]

-- | Cancel multiple partial orders.
cancelMultiplePartialOrders
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [PartialOrderInfo]
  → m (GYTxSkeleton 'PlutusV2)
cancelMultiplePartialOrders por@PORefs {..} ois = do
  gycs ← ask
  script ← mintingPolicyToScript <$> partialOrderNftPolicy por
  (cfgRef, pocd) ← fetchPartialOrderConfig porRefNft

  let (!feeOutputMap, !totalRequiredFees, !accumulatedSkeleton) =
        foldl'
          ( \(!mapAcc, !feeAcc, !skelAcc) poi@PartialOrderInfo {..} →
              let skelAdd =
                    mustHaveInput (partialOrderInfoToIn gycs por poi PartialCancel)
                      <> mustHaveOutput (partialOrderInfoToPayment poi (expectedPaymentWithDeposit poi False))
                      <> mustBeSignedBy poiOwnerKey
                      <> mustMint (GYMintReference porMintRef script) nothingRedeemer poiNFT (-1)
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
      <> mustHaveRefInput cfgRef

-- | Fills multiple orders. If the provided amount of offered tokens to buy in an order is equal to the offered amount, then we completely fill the order. Otherwise, it gets partially filled.
fillMultiplePartialOrders
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [(GYTxOutRef, Natural)]
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
  → m (GYTxSkeleton 'PlutusV2)
fillMultiplePartialOrders por ordersWithTokenBuyAmount mRefPocd = do
  let ordersWithTokenBuyAmount' = Map.fromList ordersWithTokenBuyAmount
  orders ← getPartialOrdersInfos por $ Map.keys ordersWithTokenBuyAmount'
  -- Even though we use `dropMissing`, `getPartialOrdersInfos` verify that all entries are present.
  let orders' = Map.elems $ Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched (\_ poi amt → (poi, amt))) orders ordersWithTokenBuyAmount'
  fillMultiplePartialOrders' por orders' mRefPocd

-- | Completely fill multiple orders.
completelyFillMultiplePartialOrders
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [GYTxOutRef]
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
  → m (GYTxSkeleton 'PlutusV2)
completelyFillMultiplePartialOrders por ordersRefs mRefPocd = do
  orders ← getPartialOrdersInfos por ordersRefs
  fillMultiplePartialOrders' por (map (\o → (o, poiOfferedAmount o)) $ Map.elems orders) mRefPocd

-- | Fills multiple orders. If the provided amount of offered tokens to buy in an order is equal to the offered amount, then we completely fill the order. Otherwise, it gets partially filled.
fillMultiplePartialOrders'
  ∷ (HasCallStack, GYDexApiMonad m a)
  ⇒ PORefs
  → [(PartialOrderInfo, Natural)]
  → Maybe (GYTxOutRef, PartialOrderConfigInfoF GYAddress)
  → m (GYTxSkeleton 'PlutusV2)
fillMultiplePartialOrders' por orders mRefPocd = do
  (cfgRef, pocd) ←
    case mRefPocd of
      Just (cfgRef', pocd') → pure (cfgRef', pocd')
      Nothing → fetchPartialOrderConfig (porRefNft por)
  gycs ← ask

  let buildWithFeeOutput = do
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
                  mustHaveOutput $ mkGYTxOut (pociFeeAddr pocd) fee $ datumFromPlutusData $ PartialOrderFeeOutput feeOutputMap mempty Nothing
        script ← mintingPolicyToScript <$> partialOrderNftPolicy por
        foldlM
          ( \(!prevSkel) (poi@PartialOrderInfo {..}, amt) → do
              commonCheck amt poiOfferedAmount
              cs ← validFillRangeConstraints poiStart poiEnd
              let skel =
                    if amt == poiOfferedAmount
                      then
                        let expectedValueOut = expectedPaymentWithDeposit poi True
                         in mustHaveInput (partialOrderInfoToIn gycs por poi CompleteFill)
                              <> mustHaveOutput (partialOrderInfoToPayment poi expectedValueOut)
                              <> mustMint (GYMintReference (porMintRef por) script) nothingRedeemer poiNFT (-1)
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
                         in mustHaveInput (partialOrderInfoToIn gycs por poi $ PartialFill $ toInteger amt)
                              <> mustHaveOutput o
                              <> cs

              pure $! prevSkel <> skel
          )
          (mustHaveRefInput cfgRef <> feeOutput)
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
                  <> mustHaveInput (partialOrderInfoToIn gycs por poi $ PartialFill $ toInteger amt)
                  <> mustHaveOutput o
                  <> cs
          )
          (mustHaveRefInput cfgRef)
          (zip [(1 ∷ Natural) ..] orders)

  if isJust $ find (\(PartialOrderInfo {..}, amt) → amt == poiOfferedAmount) orders
    then buildWithFeeOutput
    else buildWithoutFeeOutput
 where
  commonCheck amt poiOfferedAmount = do
    when (amt == 0) . throwAppError $ PodNonPositiveAmount $ toInteger amt
    when (amt > poiOfferedAmount) . throwAppError $ PodRequestedAmountGreaterThanOfferedAmount amt poiOfferedAmount

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
