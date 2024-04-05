module GeniusYield.Server.Dex.PartialOrder (
  OrdersAPI,
  handleOrdersApi,
) where

import Data.Ratio ((%))
import Data.Strict.Tuple qualified as Strict
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Deriving.Aeson
import Fmt
import GHC.TypeLits (AppendSymbol, Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..), cancelMultiplePartialOrders, getPartialOrdersInfos, placePartialOrder')
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Tx (handleTxSign, handleTxSubmit, throwNoSigningKeyError)
import GeniusYield.Server.Utils (addSwaggerDescription, dropSymbolAndCamelToSnake, logInfo)
import GeniusYield.Types
import RIO hiding (logDebug, logInfo)
import RIO.Map qualified as Map
import RIO.NonEmpty qualified as NonEmpty
import Servant

type BotPlaceOrderReqPrefix ∷ Symbol
type BotPlaceOrderReqPrefix = "bpop"

data BotPlaceOrderParameters = BotPlaceOrderParameters
  { bpopOfferToken ∷ !GYAssetClass,
    bpopOfferAmount ∷ !GYNatural,
    bpopPriceToken ∷ !GYAssetClass,
    bpopPriceAmount ∷ !GYNatural,
    bpopStart ∷ !(Maybe GYTime),
    bpopEnd ∷ !(Maybe GYTime)
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BotPlaceOrderReqPrefix, CamelToSnake]] BotPlaceOrderParameters

instance Swagger.ToSchema BotPlaceOrderParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @BotPlaceOrderReqPrefix}
      & addSwaggerDescription "Place order request parameters specialized towards configured bot."

newtype ChangeAddress = ChangeAddress GYAddressBech32
  deriving stock (Show, Generic)
  deriving newtype (FromJSON, ToJSON, Swagger.ToParamSchema)

instance Swagger.ToSchema ChangeAddress where
  declareNamedSchema _ = do
    addrBech32Schema ← Swagger.declareSchema (Proxy @GYAddressBech32)
    return $
      Swagger.named "ChangeAddress" $
        addrBech32Schema
          & Swagger.description
            %~ (\mt → mt <> Just " This is used as a change address by our balancer to send left over funds from selected inputs. If not provided, first address from given addresses list is used instead.")

type PlaceOrderReqPrefix ∷ Symbol
type PlaceOrderReqPrefix = "pop"

data PlaceOrderParameters = PlaceOrderParameters
  { popAddresses ∷ !(NonEmpty GYAddressBech32),
    popChangeAddress ∷ !(Maybe ChangeAddress),
    popStakeAddress ∷ !(Maybe GYStakeAddressBech32),
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

type BotCancelOrderReqPrefix ∷ Symbol
type BotCancelOrderReqPrefix = "bcop"

newtype BotCancelOrderParameters = BotCancelOrderParameters
  { bcopOrderReferences ∷ NonEmpty GYTxOutRef
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BotCancelOrderReqPrefix, CamelToSnake]] BotCancelOrderParameters

instance Swagger.ToSchema BotCancelOrderParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @BotCancelOrderReqPrefix}
      & addSwaggerDescription "Cancel order request parameters specialized towards configured bot."

type CancelOrderReqPrefix ∷ Symbol
type CancelOrderReqPrefix = "cop"

data CancelOrderParameters = CancelOrderParameters
  { copAddresses ∷ !(NonEmpty GYAddressBech32),
    copChangeAddress ∷ !(Maybe ChangeAddress),
    copCollateral ∷ !(Maybe GYTxOutRef),
    copOrderReferences ∷ !(NonEmpty GYTxOutRef)
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

type OrdersAPI =
  Summary "Build transaction to create order"
    :> Description ("Build a transaction to create an order. In case \"stakeAddress\" field is provided then order is placed at a mangled address having the given staking credential. " `AppendSymbol` CommonCollateralText)
    :> "tx"
    :> "build-open"
    :> ReqBody '[JSON] PlaceOrderParameters
    :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Create an order"
      :> Description ("Create an order. This endpoint would also sign & submit the built transaction.")
      :> ReqBody '[JSON] BotPlaceOrderParameters
      :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Build transaction to cancel order(s)"
      :> Description ("Build a transaction to cancel order(s). " `AppendSymbol` CommonCollateralText)
      :> "tx"
      :> "build-cancel"
      :> ReqBody '[JSON] CancelOrderParameters
      :> Post '[JSON] CancelOrderTransactionDetails
    :<|> Summary "Cancel order(s)"
      :> Description ("Cancel order(s). This endpoint would also sign & submit the built transaction.")
      :> ReqBody '[JSON] BotCancelOrderParameters
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
      popAddresses' = addressFromBech32 <$> popAddresses
      changeAddr = maybe (NonEmpty.head popAddresses') (\(ChangeAddress addr) → addressFromBech32 addr) popChangeAddress
  (cfgRef, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft porefs
  let unitPrice =
        rationalFromGHC $
          toInteger popPriceAmount % toInteger popOfferAmount
  txBody ←
    runSkeletonI ctx (NonEmpty.toList popAddresses') changeAddr popCollateral $
      placePartialOrder'
        porefs
        changeAddr
        (naturalToGHC popOfferAmount, popOfferToken)
        popPriceToken
        unitPrice
        popStart
        popEnd
        0
        0
        (fmap (stakeAddressToCredential . stakeAddressFromBech32) popStakeAddress)
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

resolveCtxSigningKeyInfo ∷ Ctx → IO (Strict.Pair GYSomePaymentSigningKey GYAddress)
resolveCtxSigningKeyInfo ctx = maybe throwNoSigningKeyError pure (ctxSigningKey ctx)

resolveCtxAddr ∷ Ctx → IO GYAddress
resolveCtxAddr ctx = Strict.snd <$> resolveCtxSigningKeyInfo ctx

handlePlaceOrderAndSignSubmit ∷ Ctx → BotPlaceOrderParameters → IO PlaceOrderTransactionDetails
handlePlaceOrderAndSignSubmit ctx BotPlaceOrderParameters {..} = do
  logInfo ctx "Placing an order and signing & submitting the transaction."
  ctxAddr ← addressToBech32 <$> resolveCtxAddr ctx
  details ← handlePlaceOrder ctx $ PlaceOrderParameters {popAddresses = pure ctxAddr, popChangeAddress = Just (ChangeAddress ctxAddr), popStakeAddress = ctxStakeAddress ctx, popCollateral = ctxCollateral ctx, popOfferToken = bpopOfferToken, popOfferAmount = bpopOfferAmount, popPriceToken = bpopPriceToken, popPriceAmount = bpopPriceAmount, popStart = bpopStart, popEnd = bpopEnd}
  signedTx ← handleTxSign ctx $ potdTransaction details
  txId ← handleTxSubmit ctx signedTx
  -- Though transaction id would be same, but we are returning it again, just in case...
  pure $ details {potdTransactionId = txId, potdTransaction = signedTx}

handleCancelOrder ∷ Ctx → CancelOrderParameters → IO CancelOrderTransactionDetails
handleCancelOrder ctx@Ctx {..} cops@CancelOrderParameters {..} = do
  logInfo ctx $ "Canceling order(s). Parameters: " +|| cops ||+ ""
  let porefs = dexPORefs ctxDexInfo
      copAddresses' = addressFromBech32 <$> copAddresses
      changeAddr = maybe (NonEmpty.head copAddresses') (\(ChangeAddress addr) → addressFromBech32 addr) copChangeAddress
  txBody ← runSkeletonI ctx (NonEmpty.toList copAddresses') changeAddr copCollateral $ do
    pois ← Map.elems <$> getPartialOrdersInfos porefs (NonEmpty.toList copOrderReferences)
    cancelMultiplePartialOrders porefs pois
  pure
    CancelOrderTransactionDetails
      { cotdTransaction = unsignedTx txBody,
        cotdTransactionId = txBodyTxId txBody,
        cotdTransactionFee = fromIntegral $ txBodyFee txBody
      }

handleCancelOrderAndSignSubmit ∷ Ctx → BotCancelOrderParameters → IO CancelOrderTransactionDetails
handleCancelOrderAndSignSubmit ctx BotCancelOrderParameters {..} = do
  logInfo ctx "Canceling order(s) and signing & submitting the transaction."
  ctxAddr ← addressToBech32 <$> resolveCtxAddr ctx
  details ← handleCancelOrder ctx $ CancelOrderParameters {copAddresses = pure ctxAddr, copChangeAddress = Just (ChangeAddress ctxAddr), copCollateral = ctxCollateral ctx, copOrderReferences = bcopOrderReferences}
  signedTx ← handleTxSign ctx $ cotdTransaction details
  txId ← handleTxSubmit ctx signedTx
  -- Though transaction id would be same, but we are returning it again, just in case...
  pure $ details {cotdTransactionId = txId, cotdTransaction = signedTx}
