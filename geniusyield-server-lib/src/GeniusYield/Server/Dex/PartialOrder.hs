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

type PlaceOrderReqPrefix ∷ Symbol
type PlaceOrderReqPrefix = "pop"

-- TODO: Review request and response swagger & json instances.
data PlaceOrderParameters = PlaceOrderParameters
  { popAddress ∷ !GYAddressBech32,
    popCollateral ∷ !GYTxOutRef,
    popOfferToken ∷ !GYAssetClass,
    popOfferAmount ∷ !GYNatural,
    popPriceToken ∷ !GYAssetClass,
    popPriceAmount ∷ !GYNatural,
    popStart ∷ !(Maybe GYTime),
    popEnd ∷ !(Maybe GYTime)
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix PlaceOrderReqPrefix, CamelToSnake]] PlaceOrderParameters

instance Swagger.ToSchema PlaceOrderParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @PlaceOrderReqPrefix}
      & addSwaggerDescription "Place order request parameters."

type PlaceOrderResPrefix ∷ Symbol
type PlaceOrderResPrefix = "potd"

data PlaceOrderTransactionDetails = PlaceOrderTransactionDetails
  { potdTransaction ∷ !GYTx,
    potdTransactionId ∷ !GYTxId,
    potdTransactionFee ∷ !Natural,
    potdMakerLovelaceFlatFee ∷ !Natural,
    potdMakerOfferedPercentFee ∷ !GYRational,
    potdMakerOfferedPercentFeeAmount ∷ !GYNatural,
    potdLovelaceDeposit ∷ !GYNatural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix PlaceOrderResPrefix, CamelToSnake]] PlaceOrderTransactionDetails

instance Swagger.ToSchema PlaceOrderTransactionDetails where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @PlaceOrderResPrefix}

type CancelOrderReqPrefix ∷ Symbol
type CancelOrderReqPrefix = "cop"

-- TODO: Review request and response swagger & json instances.
data CancelOrderParameters = CancelOrderParameters
  { copAddress ∷ !GYAddressBech32,
    copCollateral ∷ !GYTxOutRef,
    copOrderReferences ∷ ![GYTxOutRef]
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CancelOrderReqPrefix, CamelToSnake]] CancelOrderParameters

instance Swagger.ToSchema CancelOrderParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @CancelOrderReqPrefix}
      & addSwaggerDescription "Cancel order request parameters."

type CancelOrderResPrefix ∷ Symbol
type CancelOrderResPrefix = "cotd"

data CancelOrderTransactionDetails = CancelOrderTransactionDetails
  { cotdTransaction ∷ !GYTx,
    cotdTransactionId ∷ !GYTxId,
    cotdTransactionFee ∷ !Natural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CancelOrderResPrefix, CamelToSnake]] CancelOrderTransactionDetails

instance Swagger.ToSchema CancelOrderTransactionDetails where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @CancelOrderResPrefix}

type OrderResPrefix ∷ Symbol
type OrderResPrefix = "obi"

type OrderInfoPrefix ∷ Symbol
type OrderInfoPrefix = "oi"

data OrderInfo = OrderInfo
  { oiOfferAmount ∷ !GYNatural,
    oiPrice ∷ !GYRational,
    oiStart ∷ !(Maybe GYTime),
    oiEnd ∷ !(Maybe GYTime),
    oiOwnerAddress ∷ !GYAddressBech32,
    oiOwnerKeyHash ∷ !GYPubKeyHash,
    oiOutputReference ∷ !GYTxOutRef
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix OrderInfoPrefix, CamelToSnake]] OrderInfo

poiToOrderInfo ∷ PartialOrderInfo → OrderInfo
poiToOrderInfo PartialOrderInfo {..} =
  OrderInfo
    { oiOfferAmount = naturalFromGHC poiOfferedAmount,
      oiPrice = poiPrice,
      oiStart = poiStart,
      oiEnd = poiEnd,
      oiOwnerAddress = addressToBech32 poiOwnerAddr,
      oiOwnerKeyHash = poiOwnerKey,
      oiOutputReference = poiRef
    }

instance Swagger.ToSchema OrderInfo where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @OrderInfoPrefix}

-- TODO: Show id instead?
data OrderBookInfo = OrderBookInfo
  { obiMarketPairId ∷ !OrderAssetPair,
    obiBids ∷ ![OrderInfo],
    obiAsks ∷ ![OrderInfo]
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix OrderResPrefix, CamelToSnake]] OrderBookInfo

instance Swagger.ToSchema OrderBookInfo where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @OrderResPrefix}

-- TODO: Give timestamp for /orders
-- TODO: Rename it to `OrdersAPI`.

type DEXPartialOrderAPI =
  Summary "Create an order"
    :> Description "Create an order"
    :> "open"
    :> "tx"
    :> "generate"
    :> ReqBody '[JSON] PlaceOrderParameters
    :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Cancel order(s)"
      :> Description "Create a transaction to cancel order(s)"
      :> "cancel"
      :> "tx"
      :> "generate"
      :> ReqBody '[JSON] CancelOrderParameters
      :> Post '[JSON] CancelOrderTransactionDetails
    :<|> Summary "Get order(s)"
      :> Description "Get on-chain order(s)"
      :> Capture "market_id" OrderAssetPair
      :> QueryParam "address" GYAddressBech32
      :> Get '[JSON] OrderBookInfo

handleDEXPartialOrder ∷ Ctx → ServerT DEXPartialOrderAPI IO
handleDEXPartialOrder ctx =
  handlePlaceOrder ctx
    :<|> handleCancelOrder ctx
    :<|> handleOrders ctx

handlePlaceOrder ∷ Ctx → PlaceOrderParameters → IO PlaceOrderTransactionDetails
handlePlaceOrder ctx@Ctx {..} PlaceOrderParameters {..} = do
  logInfo ctx "Placing an order."
  -- TODO: To log request params here as well?
  let porefs = dexPORefs ctxDexInfo
      popAddress' = addressFromBech32 popAddress
  (cfgRef, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft $ porefs
  let unitPrice =
        rationalFromGHC $
          toInteger popPriceAmount % toInteger popOfferAmount
  txBody ←
    runSkeletonI ctx (pure popAddress') popAddress' (Just popCollateral) $
      placePartialOrder'
        porefs
        popAddress'
        (naturalToGHC popOfferAmount, popOfferToken)
        popPriceToken
        unitPrice
        popStart
        popEnd
        0
        0
        (addressToStakeCredential popAddress')
        cfgRef
        pocd
  pure
    PlaceOrderTransactionDetails
      { potdTransaction = unsignedTx txBody,
        potdTransactionId = txBodyTxId txBody,
        potdTransactionFee = fromIntegral $ txBodyFee txBody,
        potdMakerLovelaceFlatFee = fromIntegral $ pociMakerFeeFlat pocd,
        potdMakerOfferedPercentFee = 100 * pociMakerFeeRatio pocd,
        potdMakerOfferedPercentFeeAmount = ceiling $ toRational popOfferAmount * rationalToGHC (pociMakerFeeRatio pocd),
        potdLovelaceDeposit = fromIntegral $ pociMinDeposit pocd
      }

handleCancelOrder ∷ Ctx → CancelOrderParameters → IO CancelOrderTransactionDetails
handleCancelOrder ctx@Ctx {..} CancelOrderParameters {..} = do
  logInfo ctx "Canceling order(s)."
  -- TODO: To log request params here as well?
  let porefs = dexPORefs ctxDexInfo
      copAddress' = addressFromBech32 copAddress
  txBody ← runSkeletonI ctx (pure copAddress') copAddress' (Just copCollateral) $ do
    pois ← Map.elems <$> getPartialOrdersInfos porefs copOrderReferences
    cancelMultiplePartialOrders porefs pois
  pure
    CancelOrderTransactionDetails
      { cotdTransaction = unsignedTx txBody,
        cotdTransactionId = txBodyTxId txBody,
        cotdTransactionFee = fromIntegral $ txBodyFee txBody
      }

handleOrders ∷ Ctx → OrderAssetPair → Maybe GYAddressBech32 → IO OrderBookInfo
handleOrders ctx@Ctx {..} orderAssetPair mownAddress = do
  logInfo ctx "Fetching order(s)."
  let porefs = dexPORefs ctxDexInfo
  -- this timestamp
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
      (!bids, !asks) =
        Map.foldl'
          ( \(!accBids, !accAsks) poi@PartialOrderInfo {..} →
              let buyAP = mkOrderAssetPair poiOfferedAsset poiAskedAsset
                  poi' = poiToOrderInfo poi
               in if buyAP == orderAssetPair then (poi' : accBids, accAsks) else (accBids, poi' : accAsks)
          )
          ([], [])
          os'
  pure $
    OrderBookInfo
      { obiMarketPairId = orderAssetPair,
        obiAsks = asks,
        obiBids = bids
      }