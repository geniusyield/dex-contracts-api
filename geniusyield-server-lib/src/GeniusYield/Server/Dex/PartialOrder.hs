module GeniusYield.Server.Dex.PartialOrder (
  OrdersAPI,
  handleOrdersApi,
) where

import Data.Ratio ((%))
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..), cancelMultiplePartialOrders, getPartialOrdersInfos, placePartialOrder')
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils (addSwaggerDescription, dropSymbolAndCamelToSnake, logInfo)
import GeniusYield.Types
import RIO hiding (logDebug, logInfo)
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
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @PlaceOrderReqPrefix}
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
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @PlaceOrderResPrefix}

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
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CancelOrderReqPrefix}
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
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CancelOrderResPrefix}

type OrdersAPI =
  Summary "Build transaction to create order"
    :> Description "Build a transaction to create an order"
    :> "tx"
    :> "build-open"
    :> ReqBody '[JSON] PlaceOrderParameters
    :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Create an order"
      :> Description "Create an order. This endpoint would also sign & submit the built transaction"
      :> ReqBody '[JSON] PlaceOrderParameters
      :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Build transaction to cancel order(s)"
      :> Description "Build a transaction to cancel order(s)"
      :> "tx"
      :> "build-cancel"
      :> ReqBody '[JSON] CancelOrderParameters
      :> Post '[JSON] CancelOrderTransactionDetails
    :<|> Summary "Cancel order(s)"
      :> Description "Cancel order(s). This endpoint would also sign & submit the built transaction"
      :> ReqBody '[JSON] CancelOrderParameters
      :> Delete '[JSON] CancelOrderTransactionDetails

handleOrdersApi ∷ Ctx → ServerT OrdersAPI IO
handleOrdersApi ctx =
  handlePlaceOrder ctx
    :<|> handlePlaceOrder ctx
    :<|> handleCancelOrder ctx
    :<|> handleCancelOrder ctx

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