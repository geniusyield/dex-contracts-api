{-# LANGUAGE OverloadedLists #-}

module GeniusYield.Server.Dex.PartialOrder (
  DEXPartialOrderAPI,
  handleDEXPartialOrder,
) where

import Control.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as T
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..), PartialOrderInfo (..), cancelMultiplePartialOrders, getPartialOrdersInfos, partialOrders, placePartialOrder')
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.HTTP.Errors (
  GYApiError (..),
  IsGYApiError (..),
 )
import GeniusYield.Imports
import GeniusYield.OrderBot.Types (OrderAssetPair (..), mkEquivalentAssetPair, mkOrderAssetPair)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils (addSwaggerDescription, dropAndCamelToSnake, logInfo, unsignedTxHex)
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import Network.HTTP.Types (status400)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import RIO.Map qualified as Map
import Servant

type TradingFeesPrefix ∷ Symbol
type TradingFeesPrefix = "tf"

-- TODO: JSON & Swagger instances.
data TradingFees = TradingFees
  { tfFlatMakerFee ∷ !GYNatural,
    tfFlatTakerFee ∷ !GYNatural,
    tfPercentageMakerFee ∷ !GYRational,
    tfPercentageTakerFee ∷ !GYRational
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TradingFeesPrefix, CamelToSnake]] TradingFees

instance Swagger.ToSchema TradingFees where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @TradingFeesPrefix}
      & addSwaggerDescription "Trading fees of DEX."

type PlaceOrderReqPrefix ∷ Symbol
type PlaceOrderReqPrefix = "poreq"

-- TODO: Review request and response swagger & json instances.
data PlaceOrderRequest = PlaceOrderRequest
  { poreqAddress ∷ !GYAddressBech32,
    poreqCollateral ∷ !GYTxOutRef,
    poreqOfferToken ∷ !GYAssetClass,
    poreqOfferAmount ∷ !GYNatural,
    poreqPriceToken ∷ !GYAssetClass,
    poreqPriceAmount ∷ !GYNatural,
    poreqStart ∷ !(Maybe GYTime),
    poreqEnd ∷ !(Maybe GYTime)
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix PlaceOrderReqPrefix, CamelToSnake]] PlaceOrderRequest

instance Swagger.ToSchema PlaceOrderRequest where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @PlaceOrderReqPrefix}
      & addSwaggerDescription "Place order request parameters."

type PlaceOrderResPrefix ∷ Symbol
type PlaceOrderResPrefix = "pores"

data PlaceOrderResponse = PlaceOrderResponse
  { poresTransaction ∷ !GYTx,
    poresTransactionId ∷ !GYTxId,
    poresTransactionFee ∷ !Natural,
    poresMakerLovelaceFlatFee ∷ !Natural,
    poresMakerOfferedPercentFee ∷ !GYRational,
    poresMakerOfferedPercentFeeAmount ∷ !GYNatural,
    poresLovelaceDeposit ∷ !GYNatural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix PlaceOrderResPrefix, CamelToSnake]] PlaceOrderResponse

instance Swagger.ToSchema PlaceOrderResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @PlaceOrderResPrefix}

type CancelOrderReqPrefix ∷ Symbol
type CancelOrderReqPrefix = "coreq"

-- TODO: Review request and response swagger & json instances.
data CancelOrderRequest = CancelOrderRequest
  { coreqAddress ∷ !GYAddressBech32,
    coreqCollateral ∷ !GYTxOutRef,
    coreqOrderReferences ∷ ![GYTxOutRef]
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CancelOrderReqPrefix, CamelToSnake]] CancelOrderRequest

instance Swagger.ToSchema CancelOrderRequest where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @CancelOrderReqPrefix}
      & addSwaggerDescription "Cancel order request parameters."

type CancelOrderResPrefix ∷ Symbol
type CancelOrderResPrefix = "cores"

data CancelOrderResponse = CancelOrderResponse
  { coresTransaction ∷ !GYTx,
    coresTransactionId ∷ !GYTxId,
    coresTransactionFee ∷ !Natural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CancelOrderResPrefix, CamelToSnake]] CancelOrderResponse

instance Swagger.ToSchema CancelOrderResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @CancelOrderResPrefix}

type OrderResPrefix ∷ Symbol
type OrderResPrefix = "ores"

type MarketPairOrderPrefix ∷ Symbol
type MarketPairOrderPrefix = "mpo"

data MarketPairOrder = MarketPairOrder
  { mpoOfferAmount ∷ !GYNatural,
    mpoPrice ∷ !GYRational,
    mpoStart ∷ !(Maybe GYTime),
    mpoEnd ∷ !(Maybe GYTime),
    mpoOwnerAddress ∷ !GYAddressBech32,
    mpoOwnerKeyHash ∷ !GYPubKeyHash,
    mpoOutputReference ∷ !GYTxOutRef
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix MarketPairOrderPrefix, CamelToSnake]] MarketPairOrder

poiToMarketPairOrder ∷ PartialOrderInfo → MarketPairOrder
poiToMarketPairOrder PartialOrderInfo {..} =
  MarketPairOrder
    { mpoOfferAmount = naturalFromGHC poiOfferedAmount,
      mpoPrice = poiPrice,
      mpoStart = poiStart,
      mpoEnd = poiEnd,
      mpoOwnerAddress = addressToBech32 poiOwnerAddr,
      mpoOwnerKeyHash = poiOwnerKey,
      mpoOutputReference = poiRef
    }

instance Swagger.ToSchema MarketPairOrder where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @MarketPairOrderPrefix}

-- TODO: Show id instead?
data OrderResponse = OrderResponse
  { oresMarketPairId ∷ !OrderAssetPair,
    oresBids ∷ ![MarketPairOrder],
    oresAsks ∷ ![MarketPairOrder]
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix OrderResPrefix, CamelToSnake]] OrderResponse

instance Swagger.ToSchema OrderResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @OrderResPrefix}

type DEXPartialOrderAPI =
  "trading_fees"
    :> Get '[JSON] TradingFees
    :<|> "orders"
      :> "open"
      :> "tx"
      :> "generate"
      :> ReqBody '[JSON] PlaceOrderRequest
      :> Post '[JSON] PlaceOrderResponse
    :<|> "orders"
      :> "cancel"
      :> "tx"
      :> "generate"
      :> ReqBody '[JSON] CancelOrderRequest
      :> Post '[JSON] CancelOrderResponse
    :<|> "orders"
      :> Capture "market_id" OrderAssetPair
      :> QueryParam "address" GYAddressBech32
      :> Get '[JSON] OrderResponse

handleDEXPartialOrder ∷ Ctx → ServerT DEXPartialOrderAPI IO
handleDEXPartialOrder ctx =
  handleTradingFees ctx
    :<|> handlePlaceOrder ctx
    :<|> handleCancelOrder ctx
    :<|> handleOrders ctx

handleTradingFees ∷ Ctx → IO TradingFees
handleTradingFees ctx@Ctx {..} = do
  logInfo ctx "Calculating trading fees."
  (_, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft $ dexPORefs $ ctxDexInfo
  pure
    TradingFees
      { tfFlatMakerFee = fromIntegral $ pociMakerFeeFlat pocd,
        tfFlatTakerFee = fromIntegral $ pociTakerFee pocd,
        tfPercentageMakerFee = 100 * pociMakerFeeRatio pocd,
        tfPercentageTakerFee = 100 * pociMakerFeeRatio pocd
      }

handlePlaceOrder ∷ Ctx → PlaceOrderRequest → IO PlaceOrderResponse
handlePlaceOrder ctx@Ctx {..} PlaceOrderRequest {..} = do
  logInfo ctx "Placing an order."
  -- TODO: To log request params here as well?
  let porefs = dexPORefs ctxDexInfo
      poreqAddress' = addressFromBech32 poreqAddress
  (cfgRef, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft $ porefs
  let unitPrice =
        rationalFromGHC $
          toInteger poreqPriceAmount % toInteger poreqOfferAmount
  txBody ←
    runSkeletonI ctx (pure poreqAddress') poreqAddress' (Just poreqCollateral) $
      placePartialOrder'
        porefs
        poreqAddress'
        (naturalToGHC poreqOfferAmount, poreqOfferToken)
        poreqPriceToken
        unitPrice
        poreqStart
        poreqEnd
        0
        0
        (addressToStakeCredential poreqAddress')
        cfgRef
        pocd
  pure
    PlaceOrderResponse
      { poresTransaction = unsignedTx txBody,
        poresTransactionId = txBodyTxId txBody,
        poresTransactionFee = fromIntegral $ txBodyFee txBody,
        poresMakerLovelaceFlatFee = fromIntegral $ pociMakerFeeFlat pocd,
        poresMakerOfferedPercentFee = 100 * pociMakerFeeRatio pocd,
        poresMakerOfferedPercentFeeAmount = ceiling $ toRational poreqOfferAmount * rationalToGHC (pociMakerFeeRatio pocd),
        poresLovelaceDeposit = fromIntegral $ pociMinDeposit pocd
      }

handleCancelOrder ∷ Ctx → CancelOrderRequest → IO CancelOrderResponse
handleCancelOrder ctx@Ctx {..} CancelOrderRequest {..} = do
  logInfo ctx "Canceling order(s)."
  -- TODO: To log request params here as well?
  let porefs = dexPORefs ctxDexInfo
      coreqAddress' = addressFromBech32 coreqAddress
  txBody ← runSkeletonI ctx (pure coreqAddress') coreqAddress' (Just coreqCollateral) $ do
    pois ← Map.elems <$> getPartialOrdersInfos porefs coreqOrderReferences
    cancelMultiplePartialOrders porefs pois
  pure
    CancelOrderResponse
      { coresTransaction = unsignedTx txBody,
        coresTransactionId = txBodyTxId txBody,
        coresTransactionFee = fromIntegral $ txBodyFee txBody
      }

handleOrders ∷ Ctx → OrderAssetPair → Maybe GYAddressBech32 → IO OrderResponse
handleOrders ctx@Ctx {..} orderAssetPair mownAddress = do
  logInfo ctx "Fetching order(s)."
  let porefs = dexPORefs ctxDexInfo
  os ← runQuery ctx $ partialOrders porefs
  let os' =
        Map.filter
          ( \PartialOrderInfo {..} →
              let ap1 = mkOrderAssetPair poiOfferedAsset poiAskedAsset
                  ap2 = mkEquivalentAssetPair ap1
               in (ap1 == orderAssetPair || ap2 == orderAssetPair)
                    && case mownAddress of Nothing → True; Just ownAddress → poiOwnerKey == fromJust (addressToPubKeyHash $ addressFromBech32 ownAddress) -- TODO: Get rid of `fromJust`.
          )
          os
      -- TODO: Make it strict, likely there is memory leak here.
      -- TODO: Check if it's implementation is correct.
      (bids, asks) =
        Map.foldl'
          ( \(!accBids, !accAsks) poi@PartialOrderInfo {..} →
              let buyAP = mkOrderAssetPair poiOfferedAsset poiAskedAsset
                  poi' = poiToMarketPairOrder poi
               in if buyAP == orderAssetPair then (poi' : accBids, accAsks) else (accBids, poi' : accAsks)
          )
          ([], [])
          os'
  pure $
    OrderResponse
      { oresMarketPairId = orderAssetPair,
        oresAsks = asks,
        oresBids = bids
      }