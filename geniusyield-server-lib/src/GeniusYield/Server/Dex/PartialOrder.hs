module GeniusYield.Server.Dex.PartialOrder (
  OrdersAPI,
  handleOrdersApi,
) where

import Data.Ratio ((%))
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import Fmt
import GHC.TypeLits (AppendSymbol, Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..), cancelMultiplePartialOrders, getPartialOrdersInfos, placePartialOrder')
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Tx (handleTxSign, handleTxSubmit)
import GeniusYield.Server.Utils (addSwaggerDescription, dropSymbolAndCamelToSnake, logInfo)
import GeniusYield.Types
import RIO hiding (logDebug, logInfo)
import RIO.Map qualified as Map
import Servant

type PlaceOrderReqPrefix ∷ Symbol
type PlaceOrderReqPrefix = "pop"

data PlaceOrderParameters = PlaceOrderParameters
  { popAddress ∷ !GYAddressBech32,
    popCollateral ∷ !(Maybe GYTxOutRef),
    popOfferToken ∷ !GYAssetClass,
    popOfferAmount ∷ !GYNatural,
    popPriceToken ∷ !GYAssetClass,
    popPriceAmount ∷ !GYNatural,
    popStart ∷ !(Maybe GYTime),
    popEnd ∷ !(Maybe GYTime)
  }
  deriving stock (Show, Generic)
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

data CancelOrderParameters = CancelOrderParameters
  { copAddress ∷ !GYAddressBech32,
    copCollateral ∷ !(Maybe GYTxOutRef),
    copOrderReferences ∷ ![GYTxOutRef]
  }
  deriving stock (Show, Generic)
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

type CommonCollateralText ∷ Symbol
type CommonCollateralText = "Note that if \"collateral\" field is not provided, then framework would try to pick collateral UTxO on it's own and in that case would also be free to spend it (i.e., would be made available to coin balancer)."

type CommonCollateralTextForSign ∷ Symbol
type CommonCollateralTextForSign = "Note that if \"collateral\" field is not provided, then the default collateral provided in the configuration is used and if collateral is not provided in the configuration also then framework would try to pick collateral UTxO on it's own and in that case would also be free to spend it (i.e., would be made available to coin balancer)."

type OrdersAPI =
  Summary "Build transaction to create order"
    :> Description ("Build a transaction to create an order. Order is placed at a mangled address where staking credential is that of the given \"address\" field. " `AppendSymbol` CommonCollateralText)
    :> "tx"
    :> "build-open"
    :> ReqBody '[JSON] PlaceOrderParameters
    :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Create an order"
      :> Description ("Create an order. This endpoint would also sign & submit the built transaction. Order is placed at a mangled address where staking credential is that of the given \"address\" field. " `AppendSymbol` CommonCollateralTextForSign)
      :> ReqBody '[JSON] PlaceOrderParameters
      :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Build transaction to cancel order(s)"
      :> Description ("Build a transaction to cancel order(s). " `AppendSymbol` CommonCollateralText)
      :> "tx"
      :> "build-cancel"
      :> ReqBody '[JSON] CancelOrderParameters
      :> Post '[JSON] CancelOrderTransactionDetails
    :<|> Summary "Cancel order(s)"
      :> Description ("Cancel order(s). This endpoint would also sign & submit the built transaction. " `AppendSymbol` CommonCollateralTextForSign)
      :> ReqBody '[JSON] CancelOrderParameters
      :> Delete '[JSON] CancelOrderTransactionDetails

handleOrdersApi ∷ Ctx → ServerT OrdersAPI IO
handleOrdersApi ctx =
  handlePlaceOrder ctx
    :<|> handlePlaceOrderAndSignSubmit ctx
    :<|> handleCancelOrder ctx
    :<|> handleCancelOrderAndSignSubmit ctx

handlePlaceOrder ∷ Ctx → PlaceOrderParameters → IO PlaceOrderTransactionDetails
handlePlaceOrder ctx@Ctx {..} pops@PlaceOrderParameters {..} = do
  logInfo ctx $ "Placing an order. Parameters: " +|| pops ||+ ""
  let porefs = dexPORefs ctxDexInfo
      popAddress' = addressFromBech32 popAddress
  (cfgRef, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft porefs
  let unitPrice =
        rationalFromGHC $
          toInteger popPriceAmount % toInteger popOfferAmount
  txBody ←
    runSkeletonI ctx (pure popAddress') popAddress' (popCollateral <|> ctxCollateral) $
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

handlePlaceOrderAndSignSubmit ∷ Ctx → PlaceOrderParameters → IO PlaceOrderTransactionDetails
handlePlaceOrderAndSignSubmit ctx pops = do
  logInfo ctx "Placing an order and signing & submitting the transaction."
  details ← handlePlaceOrder ctx pops
  signedTx ← handleTxSign ctx $ potdTransaction details
  txId ← handleTxSubmit ctx signedTx
  -- Though transaction id would be same, but we are returning it again, just in case...
  pure $ details {potdTransactionId = txId, potdTransaction = signedTx}

handleCancelOrder ∷ Ctx → CancelOrderParameters → IO CancelOrderTransactionDetails
handleCancelOrder ctx@Ctx {..} cops@CancelOrderParameters {..} = do
  logInfo ctx $ "Canceling order(s). Parameters: " +|| cops ||+ ""
  let porefs = dexPORefs ctxDexInfo
      copAddress' = addressFromBech32 copAddress
  txBody ← runSkeletonI ctx (pure copAddress') copAddress' (copCollateral <|> ctxCollateral) $ do
    pois ← Map.elems <$> getPartialOrdersInfos porefs copOrderReferences
    cancelMultiplePartialOrders porefs pois
  pure
    CancelOrderTransactionDetails
      { cotdTransaction = unsignedTx txBody,
        cotdTransactionId = txBodyTxId txBody,
        cotdTransactionFee = fromIntegral $ txBodyFee txBody
      }

handleCancelOrderAndSignSubmit ∷ Ctx → CancelOrderParameters → IO CancelOrderTransactionDetails
handleCancelOrderAndSignSubmit ctx cops = do
  logInfo ctx "Canceling order(s) and signing & submitting the transaction."
  details ← handleCancelOrder ctx cops
  signedTx ← handleTxSign ctx $ cotdTransaction details
  txId ← handleTxSubmit ctx signedTx
  -- Though transaction id would be same, but we are returning it again, just in case...
  pure $ details {cotdTransactionId = txId, cotdTransaction = signedTx}
