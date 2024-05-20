module GeniusYield.Server.Dex.PartialOrder (
  OrdersAPI,
  handleOrdersApi,
  OrderInfo (..),
  poiToOrderInfo,
  PodServerException (..),
  PodOrderNotFound (..),
) where

import Data.Aeson (ToJSON (..))
import Data.Ratio ((%))
import Data.Strict.Tuple (Pair (..))
import Data.Strict.Tuple qualified as Strict
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Deriving.Aeson
import Fmt
import GHC.TypeLits (AppendSymbol, Symbol)
import GeniusYield.Api.Dex.PartialOrder (PartialOrderInfo (..), cancelMultiplePartialOrders', fillMultiplePartialOrders', getPartialOrdersInfos, getPartialOrdersInfos', getVersionsInOrders, orderByNft, partialOrderPrice', placePartialOrder'', preferentiallySelectLatestPocd, preferentiallySelectLatestVersion, roundFunctionForPOCVersion)
import GeniusYield.Api.Dex.PartialOrderConfig (RefPocd (..), SomeRefPocd (SomeRefPocd), fetchPartialOrderConfig, fetchPartialOrderConfigs)
import GeniusYield.HTTP.Errors
import GeniusYield.OrderBot.Domain.Markets (OrderAssetPair (..))
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Scripts.Dex.Version (POCVersion (POCVersion1_1))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Tx (handleTxSign, handleTxSubmit, throwNoSigningKeyError)
import GeniusYield.Server.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake, logDebug, logInfo)
import GeniusYield.Types
import Network.HTTP.Types.Status
import RIO hiding (logDebug, logInfo)
import RIO.Map qualified as Map
import RIO.NonEmpty qualified as NonEmpty
import RIO.Text qualified as T
import Servant

-- | Number of orders that we at most allow to be filled in a single transaction.
maxFillOrders ∷ GYNatural
maxFillOrders = 5

data PodServerException
  = -- | Cannot fill more than allowed number of orders.
    PodMultiFillMoreThanAllowed
  | -- | When filling multiple orders, they all should have same payment token so that taker fee is charged in only one token.
    PodMultiFillNotAllSamePaymentToken
  deriving stock (Show)
  deriving anyclass (Exception)

-- | When order whose details is queried for is not found.
data PodOrderNotFound = PodOrderNotFound
  deriving (Eq, Show, Generic)
  deriving anyclass (Exception, ToJSON)

instance Swagger.ToSchema PodOrderNotFound where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription (toErrDescription PodOrderNotFound)

class ErrDescription e where
  toErrDescription ∷ e → Text

instance ErrDescription PodOrderNotFound where
  toErrDescription _ = "Order not found"

instance IsGYApiError PodServerException where
  toApiError PodMultiFillMoreThanAllowed =
    GYApiError
      { gaeErrorCode = "MULTI_FILL_MORE_THAN_ALLOWED",
        gaeHttpStatus = status400,
        gaeMsg = T.pack $ "Orders to fill is more than " <> show maxFillOrders
      }
  toApiError PodMultiFillNotAllSamePaymentToken =
    GYApiError
      { gaeErrorCode = "MULTI_FILL_NOT_SAME_PAIR",
        gaeHttpStatus = status400,
        gaeMsg = "Given orders are not having same payment token"
      }

instance IsGYApiError PodOrderNotFound where
  toApiError PodOrderNotFound =
    GYApiError
      { gaeErrorCode = "ORDER_NOT_FOUND",
        gaeHttpStatus = status404,
        gaeMsg = toErrDescription PodOrderNotFound
      }

type OrderInfoPrefix ∷ Symbol
type OrderInfoPrefix = "oi"

data OrderInfo = OrderInfo
  { oiOfferAmount ∷ !GYRational,
    oiPrice ∷ !GYRational,
    oiStart ∷ !(Maybe GYTime),
    oiEnd ∷ !(Maybe GYTime),
    oiOwnerAddress ∷ !GYAddressBech32,
    oiOwnerKeyHash ∷ !GYPubKeyHash,
    oiOutputReference ∷ !GYTxOutRef,
    oiNFTToken ∷ !GYAssetClass
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix OrderInfoPrefix, CamelToSnake]] OrderInfo

instance Swagger.ToSchema OrderInfo where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @OrderInfoPrefix}

type OrderInfoDetailedPrefix ∷ Symbol
type OrderInfoDetailedPrefix = "oid"

data OrderInfoDetailed = OrderInfoDetailed
  { oidOfferAmount ∷ !GYNatural,
    oidOriginalOfferAmount ∷ !GYNatural,
    oidOfferAsset ∷ !GYAssetClass,
    oidAskedAsset ∷ !GYAssetClass,
    oidPrice ∷ !GYRational,
    oidPartialFills ∷ !GYNatural,
    oidContainedAskedTokens ∷ !GYNatural,
    oidStart ∷ !(Maybe GYTime),
    oidEnd ∷ !(Maybe GYTime),
    oidOwnerAddress ∷ !GYAddressBech32,
    oidOwnerKeyHash ∷ !GYPubKeyHash,
    oidOutputReference ∷ !GYTxOutRef,
    oidNFTToken ∷ !GYAssetClass
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix OrderInfoDetailedPrefix, CamelToSnake]] OrderInfoDetailed

instance Swagger.ToSchema OrderInfoDetailed where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @OrderInfoDetailedPrefix}

poiToOrderInfo ∷ PartialOrderInfo → OrderAssetPair → Pair OrderInfo Bool
poiToOrderInfo PartialOrderInfo {..} oap =
  let isSell = commodityAsset oap == poiOfferedAsset
      poiOfferedAmount' = fromIntegral poiOfferedAmount
   in OrderInfo
        { oiOfferAmount = if isSell then poiOfferedAmount' else poiOfferedAmount' * poiPrice,
          oiPrice = if isSell then poiPrice else 1 / poiPrice,
          oiStart = poiStart,
          oiEnd = poiEnd,
          oiOwnerAddress = addressToBech32 poiOwnerAddr,
          oiOwnerKeyHash = poiOwnerKey,
          oiOutputReference = poiRef,
          oiNFTToken = GYToken poiNFTCS poiNFT
        }
        :!: isSell

poiToOrderInfoDetailed ∷ PartialOrderInfo → OrderInfoDetailed
poiToOrderInfoDetailed PartialOrderInfo {..} =
  OrderInfoDetailed
    { oidOfferAmount = naturalFromGHC poiOfferedAmount,
      oidOriginalOfferAmount = naturalFromGHC poiOfferedOriginalAmount,
      oidOfferAsset = poiOfferedAsset,
      oidAskedAsset = poiAskedAsset,
      oidPrice = poiPrice,
      oidPartialFills = naturalFromGHC poiPartialFills,
      oidContainedAskedTokens = naturalFromGHC poiContainedPayment,
      oidStart = poiStart,
      oidEnd = poiEnd,
      oidOwnerAddress = addressToBech32 poiOwnerAddr,
      oidOwnerKeyHash = poiOwnerKey,
      oidOutputReference = poiRef,
      oidNFTToken = GYToken poiNFTCS poiNFT
    }

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
    potdTransactionFee ∷ !GYNatural,
    potdMakerLovelaceFlatFee ∷ !GYNatural,
    potdMakerOfferedPercentFee ∷ !GYRational,
    potdMakerOfferedPercentFeeAmount ∷ !GYNatural,
    potdLovelaceDeposit ∷ !GYNatural,
    potdOrderRef ∷ !GYTxOutRef,
    potdNFTToken ∷ !GYAssetClass
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
    cotdTransactionFee ∷ !GYNatural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CancelOrderResPrefix, CamelToSnake]] CancelOrderTransactionDetails

instance Swagger.ToSchema CancelOrderTransactionDetails where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CancelOrderResPrefix}

type FillOrderReqPrefix ∷ Symbol
type FillOrderReqPrefix = "fop"

data FillOrderParameters = FillOrderParameters
  { fopAddresses ∷ !(NonEmpty GYAddressBech32),
    fopChangeAddress ∷ !(Maybe ChangeAddress),
    fopCollateral ∷ !(Maybe GYTxOutRef),
    fopOrderReferencesWithAmount ∷ !(NonEmpty (GYTxOutRef, GYNatural))
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix FillOrderReqPrefix, CamelToSnake]] FillOrderParameters

instance Swagger.ToSchema FillOrderParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @FillOrderReqPrefix}
      & addSwaggerDescription "Fill order(s) request parameters."
      & addSwaggerExample (toJSON $ FillOrderParameters {fopAddresses = pure "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5", fopChangeAddress = Just (ChangeAddress "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"), fopCollateral = Just "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1", fopOrderReferencesWithAmount = ("0018dbaa1611531b9f11a31765e8abe875f9c43750b82b5f321350f31e1ea747#0", 100) :| [("0018dbaa1611531b9f11a31765e8abe875f9c43750b82b5f321350f31e144444#0", 100)]})

type FillOrderResPrefix ∷ Symbol
type FillOrderResPrefix = "fotd"

data FillOrderTransactionDetails = FillOrderTransactionDetails
  { fotdTransaction ∷ !GYTx,
    fotdTransactionId ∷ !GYTxId,
    fotdTransactionFee ∷ !GYNatural,
    fotdTakerLovelaceFlatFee ∷ !GYNatural,
    fotdTakerOfferedPercentFee ∷ !GYRational,
    fotdTakerOfferedPercentFeeAmount ∷ !GYNatural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix FillOrderResPrefix, CamelToSnake]] FillOrderTransactionDetails

instance Swagger.ToSchema FillOrderTransactionDetails where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @FillOrderResPrefix}

type CommonCollateralText ∷ Symbol
type CommonCollateralText = "Note that if \"collateral\" field is not provided, then framework would try to pick collateral UTxO on it's own and in that case would also be free to spend it (i.e., would be made available to coin balancer)."

type CommonSignText ∷ Symbol
type CommonSignText = "It uses the signing key from configuration to compute for wallet address. If collateral is specified in the configuration, then it would be used for."

type OrdersAPI =
  Summary "Build transaction to create order"
    :> Description ("Build a transaction to create an order. In case \"stakeAddress\" field is provided then order is placed at a mangled address having the given staking credential. " `AppendSymbol` CommonCollateralText)
    :> "tx"
    :> "build-open"
    :> ReqBody '[JSON] PlaceOrderParameters
    :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Create an order"
      :> Description ("Create an order. This endpoint would also sign & submit the built transaction. " `AppendSymbol` CommonSignText `AppendSymbol` " \"stakeAddress\" field from configuration, if provided, is used to place order at a mangled address.")
      :> ReqBody '[JSON] BotPlaceOrderParameters
      :> Post '[JSON] PlaceOrderTransactionDetails
    :<|> Summary "Build transaction to cancel order(s)"
      :> Description ("Build a transaction to cancel order(s). " `AppendSymbol` CommonCollateralText)
      :> "tx"
      :> "build-cancel"
      :> ReqBody '[JSON] CancelOrderParameters
      :> Post '[JSON] CancelOrderTransactionDetails
    :<|> Summary "Cancel order(s)"
      :> Description ("Cancel order(s). This endpoint would also sign & submit the built transaction. " `AppendSymbol` CommonSignText)
      :> ReqBody '[JSON] BotCancelOrderParameters
      :> Delete '[JSON] CancelOrderTransactionDetails
    :<|> Summary "Get order(s) details"
      :> Description "Get details of order(s) using their unique NFT token. Note that each order is identified uniquely by an associated NFT token which can then later be used to retrieve it's details across partial fills."
      :> "details"
      :> ReqBody '[JSON] [GYAssetClass]
      :> Post '[JSON] [OrderInfoDetailed]
    :<|> Summary "Get order details"
      :> Description "Get details of an order using it's unique NFT token. Note that each order is identified uniquely by an associated NFT token which can then later be used to retrieve it's details across partial fills."
      :> "details"
      :> Capture "nft-token" GYAssetClass
      :> UVerb 'GET '[JSON] '[WithStatus 200 OrderInfoDetailed, WithStatus 404 PodOrderNotFound]
    :<|> Summary "Build transaction to fill order(s)"
      :> Description ("Build a transaction to fill order(s). " `AppendSymbol` CommonCollateralText)
      :> "tx"
      :> "build-fill"
      :> ReqBody '[JSON] FillOrderParameters
      :> Post '[JSON] FillOrderTransactionDetails

handleOrdersApi ∷ Ctx → ServerT OrdersAPI IO
handleOrdersApi ctx =
  handlePlaceOrder ctx
    :<|> handlePlaceOrderAndSignSubmit ctx
    :<|> handleCancelOrders ctx
    :<|> handleCancelOrdersAndSignSubmit ctx
    :<|> handleOrdersDetails ctx
    :<|> handleOrderDetails ctx
    :<|> handleFillOrders ctx

handlePlaceOrder ∷ Ctx → PlaceOrderParameters → IO PlaceOrderTransactionDetails
handlePlaceOrder ctx@Ctx {..} pops@PlaceOrderParameters {..} = do
  logInfo ctx $ "Placing an order. Parameters: " +|| pops ||+ ""
  let porefs = dexPORefs ctxDexInfo
      popAddresses' = addressFromBech32 <$> popAddresses
      changeAddr = maybe (NonEmpty.head popAddresses') (\(ChangeAddress addr) → addressFromBech32 addr) popChangeAddress
      pocVersion = POCVersion1_1
  SomeRefPocd (RefPocd (cfgRef :!: pocd)) ← runQuery ctx $ fetchPartialOrderConfig pocVersion porefs
  let unitPrice =
        rationalFromGHC $
          toInteger popPriceAmount % toInteger popOfferAmount
  (nftAC, txBody) ←
    runSkeletonF ctx (NonEmpty.toList popAddresses') changeAddr popCollateral $
      placePartialOrder''
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
  let txId = txBodyTxId txBody
  pure
    PlaceOrderTransactionDetails
      { potdTransaction = unsignedTx txBody,
        potdTransactionId = txId,
        potdTransactionFee = fromIntegral $ txBodyFee txBody,
        potdMakerLovelaceFlatFee = fromIntegral $ pociMakerFeeFlat pocd,
        potdMakerOfferedPercentFee = 100 * pociMakerFeeRatio pocd,
        potdMakerOfferedPercentFeeAmount = roundFunctionForPOCVersion pocVersion $ toRational popOfferAmount * rationalToGHC (pociMakerFeeRatio pocd),
        potdLovelaceDeposit = fromIntegral $ pociMinDeposit pocd,
        potdOrderRef = txOutRefFromTuple (txId, 0),
        potdNFTToken = nftAC
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

handleCancelOrders ∷ Ctx → CancelOrderParameters → IO CancelOrderTransactionDetails
handleCancelOrders ctx@Ctx {..} cops@CancelOrderParameters {..} = do
  logInfo ctx $ "Canceling order(s). Parameters: " +|| cops ||+ ""
  let porefs = dexPORefs ctxDexInfo
      copAddresses' = addressFromBech32 <$> copAddresses
      changeAddr = maybe (NonEmpty.head copAddresses') (\(ChangeAddress addr) → addressFromBech32 addr) copChangeAddress
  txBody ← runSkeletonI ctx (NonEmpty.toList copAddresses') changeAddr copCollateral $ do
    pois ← Map.elems <$> getPartialOrdersInfos porefs (NonEmpty.toList copOrderReferences)
    cancelMultiplePartialOrders' porefs pois
  pure
    CancelOrderTransactionDetails
      { cotdTransaction = unsignedTx txBody,
        cotdTransactionId = txBodyTxId txBody,
        cotdTransactionFee = fromIntegral $ txBodyFee txBody
      }

handleCancelOrdersAndSignSubmit ∷ Ctx → BotCancelOrderParameters → IO CancelOrderTransactionDetails
handleCancelOrdersAndSignSubmit ctx BotCancelOrderParameters {..} = do
  logInfo ctx "Canceling order(s) and signing & submitting the transaction."
  ctxAddr ← addressToBech32 <$> resolveCtxAddr ctx
  details ← handleCancelOrders ctx $ CancelOrderParameters {copAddresses = pure ctxAddr, copChangeAddress = Just (ChangeAddress ctxAddr), copCollateral = ctxCollateral ctx, copOrderReferences = bcopOrderReferences}
  signedTx ← handleTxSign ctx $ cotdTransaction details
  txId ← handleTxSubmit ctx signedTx
  -- Though transaction id would be same, but we are returning it again, just in case...
  pure $ details {cotdTransactionId = txId, cotdTransaction = signedTx}

handleOrderDetails ∷ Ctx → GYAssetClass → IO (Union '[WithStatus 200 OrderInfoDetailed, WithStatus 404 PodOrderNotFound])
handleOrderDetails ctx@Ctx {..} ac = do
  logInfo ctx $ "Getting order details for NFT token: " +|| ac ||+ ""
  let porefs = dexPORefs ctxDexInfo
  os ← runQuery ctx $ fmap poiToOrderInfoDetailed <$> orderByNft porefs ac
  case os of
    Nothing → throwIO PodOrderNotFound -- We could use `respond` here as well but then as it would not have @application/json@ header, it would not be caught by our @errorJsonWrapMiddleware@.
    Just o → respond (WithStatus @200 o)

handleOrdersDetails ∷ Ctx → [GYAssetClass] → IO [OrderInfoDetailed]
handleOrdersDetails ctx@Ctx {..} acs = do
  logInfo ctx $ "Getting orders details for NFT tokens: " +|| acs ||+ ""
  let porefs = dexPORefs ctxDexInfo
  os ← forM acs $ \ac → do
    logDebug ctx $ "Getting order details for NFT token: " +|| ac ||+ ""
    runQuery ctx $
      fmap poiToOrderInfoDetailed <$> orderByNft porefs ac
  pure $ catMaybes os

handleFillOrders ∷ Ctx → FillOrderParameters → IO FillOrderTransactionDetails
handleFillOrders ctx@Ctx {..} fops@FillOrderParameters {..} = do
  logInfo ctx $ "Filling order(s). Parameters: " +|| fops ||+ ""
  let porefs = dexPORefs ctxDexInfo
  refPocds ← runQuery ctx $ fetchPartialOrderConfigs porefs
  ordersWithTokenBuyAmount ← runQuery ctx $ getPartialOrdersInfos' porefs $ NonEmpty.toList $ second naturalToGHC <$> fopOrderReferencesWithAmount
  when (length ordersWithTokenBuyAmount > fromIntegral maxFillOrders) $ throwIO PodMultiFillMoreThanAllowed
  let versionsSet = getVersionsInOrders $ map fst ordersWithTokenBuyAmount
      overallPocVersion = preferentiallySelectLatestVersion versionsSet
      pocd = preferentiallySelectLatestPocd versionsSet refPocds
      takerFeeRatio = pociMakerFeeRatio pocd
      takerFee = computePercentTakerFees overallPocVersion ordersWithTokenBuyAmount takerFeeRatio
      maxFlatTakerFee = foldl' (\prevMax (poi, _) → max prevMax $ poiTakerLovelaceFlatFee poi) 0 ordersWithTokenBuyAmount
      fopAddresses' = addressFromBech32 <$> fopAddresses
      changeAddr = maybe (NonEmpty.head fopAddresses') (\(ChangeAddress addr) → addressFromBech32 addr) fopChangeAddress
  takerFee' ← case valueToList takerFee of
    [(_, feeAmt)] → pure $ fromIntegral feeAmt
    _ → throwIO PodMultiFillNotAllSamePaymentToken
  txBody ← runSkeletonI ctx (NonEmpty.toList fopAddresses') changeAddr fopCollateral $ do
    fillMultiplePartialOrders' porefs ordersWithTokenBuyAmount (Just refPocds) takerFee
  pure
    FillOrderTransactionDetails
      { fotdTransaction = unsignedTx txBody,
        fotdTransactionId = txBodyTxId txBody,
        fotdTransactionFee = fromIntegral $ txBodyFee txBody,
        fotdTakerLovelaceFlatFee = fromIntegral maxFlatTakerFee,
        fotdTakerOfferedPercentFee = 100 * takerFeeRatio,
        fotdTakerOfferedPercentFeeAmount = takerFee'
      }
 where
  computePercentTakerFees ∷ Foldable t ⇒ POCVersion → t (PartialOrderInfo, Natural) → GYRational → GYValue
  computePercentTakerFees overallPocVersion ordersWithTokenBuyAmount takerFeeRatio =
    let takerACWithAmt =
          foldl'
            ( \accTakerACWithAmt (poi@PartialOrderInfo {..}, amtToFill) →
                let takerOfferedAmount = partialOrderPrice' poi amtToFill
                 in Map.insertWith (+) poiAskedAsset takerOfferedAmount accTakerACWithAmt
            )
            mempty
            ordersWithTokenBuyAmount
        takerFee =
          Map.foldlWithKey' (\acc ac amt → acc <> valueSingleton ac (roundFunctionForPOCVersion overallPocVersion $ toRational amt * rationalToGHC takerFeeRatio)) mempty takerACWithAmt
     in takerFee
