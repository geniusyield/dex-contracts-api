module GeniusYield.Server.Api (
  GeniusYieldAPI,
  geniusYieldAPI,
  geniusYieldServer,
  MainAPI,
  mainAPI,
  mainServer,
  geniusYieldAPISwagger,
) where

import Control.Lens (at, (?~))
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Strict.Tuple
import Data.Swagger
import Data.Swagger qualified as Swagger
import Data.Version (showVersion)
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..), PartialOrderInfo (..), partialOrders)
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.OrderBot.Types (OrderAssetPair (..), equivalentAssetPair, mkEquivalentAssetPair, mkOrderAssetPair)
import GeniusYield.Scripts (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Assets
import GeniusYield.Server.Auth (ApiKeyHeader, apiKeyHeaderText)
import GeniusYield.Server.Constants (gitHash)
import GeniusYield.Server.Ctx
import GeniusYield.Server.Dex.HistoricalPrices.Maestro
import GeniusYield.Server.Dex.Markets (MarketsAPI, handleMarketsApi)
import GeniusYield.Server.Dex.PartialOrder (OrdersAPI, handleOrdersApi)
import GeniusYield.Server.Tx (TxAPI, handleTxApi)
import GeniusYield.Server.Utils
import GeniusYield.TxBuilder (GYTxQueryMonad (utxosAtAddress))
import GeniusYield.Types
import PackageInfo_geniusyield_server_lib qualified as PackageInfo
import RIO hiding (asks, logDebug, logInfo)
import RIO.Char (toLower)
import RIO.List (isPrefixOf)
import RIO.Map qualified as Map
import Servant
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Swagger

-------------------------------------------------------------------------------
-- Settings.
-------------------------------------------------------------------------------

type SettingsPrefix ∷ Symbol
type SettingsPrefix = "settings"

data Settings = Settings
  { settingsNetwork ∷ !String,
    settingsVersion ∷ !String,
    settingsRevision ∷ !String,
    settingsBackend ∷ !String
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SettingsPrefix, CamelToSnake]] Settings

instance Swagger.ToSchema Settings where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SettingsPrefix}
      & addSwaggerDescription "Genius Yield Server settings."

type TradingFeesPrefix ∷ Symbol
type TradingFeesPrefix = "tf"

-------------------------------------------------------------------------------
-- Trading fees.
-------------------------------------------------------------------------------

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
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @TradingFeesPrefix}
      & addSwaggerDescription "Trading fees of DEX."

-------------------------------------------------------------------------------
-- Order book.
-------------------------------------------------------------------------------

type OrderResPrefix ∷ Symbol
type OrderResPrefix = "obi"

type OrderInfoPrefix ∷ Symbol
type OrderInfoPrefix = "oi"

data OrderInfo = OrderInfo
  { oiOfferAmount ∷ !GYRational,
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
          oiOutputReference = poiRef
        }
        :!: isSell

instance Swagger.ToSchema OrderInfo where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @OrderInfoPrefix}

data OrderBookInfo = OrderBookInfo
  { obiMarketPairId ∷ !OrderAssetPair,
    obiTimestamp ∷ !GYTime,
    obiBids ∷ ![OrderInfo],
    obiAsks ∷ ![OrderInfo]
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix OrderResPrefix, CamelToSnake]] OrderBookInfo

instance Swagger.ToSchema OrderBookInfo where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @OrderResPrefix}

-------------------------------------------------------------------------------
-- Server's API.
-------------------------------------------------------------------------------

type SettingsAPI = Summary "Server settings" :> Description "Get server settings such as network, version, and revision" :> Get '[JSON] Settings

type TradingFeesAPI =
  Summary "Trading fees"
    :> Description "Get trading fees of DEX."
    :> Get '[JSON] TradingFees

type OrderBookAPI = Summary "Order book" :> Description "Get order book for a specific market." :> Capture "market-id" OrderAssetPair :> QueryParam "address" GYAddressBech32 :> Get '[JSON] OrderBookInfo

type BalancesAPI = Summary "Balances" :> Description "Get token balances of an address." :> Capture "address" GYAddressBech32 :> Get '[JSON] GYValue

type V0API =
  "settings" :> SettingsAPI
    :<|> "orders" :> OrdersAPI
    :<|> "markets" :> MarketsAPI
    :<|> "tx" :> TxAPI
    :<|> "trading-fees" :> TradingFeesAPI
    :<|> "assets" :> AssetsAPI
    :<|> "order-book" :> OrderBookAPI
    :<|> "historical-prices" :> "maestro" :> MaestroPriceHistoryAPI
    :<|> "balances" :> BalancesAPI

type V0 ∷ Symbol
type V0 = "v0"

type APIKeyAuthProtect = AuthProtect ApiKeyHeader

type GeniusYieldAPI = APIKeyAuthProtect :> V0 :> V0API

geniusYieldAPI ∷ Proxy GeniusYieldAPI
geniusYieldAPI = Proxy

infixr 4 +>

type family (+>) (api1 ∷ k) (api2 ∷ Type) where
  (+>) api1 api2 = APIKeyAuthProtect :> V0 :> api1 :> api2

apiKeySecurityScheme ∷ SecurityScheme
apiKeySecurityScheme =
  SecurityScheme
    { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams apiKeyHeaderText ApiKeyHeader),
      _securitySchemeDescription = Just "API key for accessing the server's API."
    }

type instance AuthServerData APIKeyAuthProtect = ()

instance HasSwagger api ⇒ HasSwagger (APIKeyAuthProtect :> api) where
  toSwagger _ =
    toSwagger (Proxy ∷ Proxy api)
      & securityDefinitions
      .~ SecurityDefinitions (IOHM.fromList [(apiKeyHeaderText, apiKeySecurityScheme)])
      & allOperations
      . security
      .~ [SecurityRequirement (IOHM.singleton apiKeyHeaderText [])]
      & allOperations
      . responses
      %~ addCommonResponses

addCommonResponses ∷ Responses → Responses
addCommonResponses resps = resps & at 401 ?~ Inline response401 & at 403 ?~ Inline response403

response401 ∷ Response
response401 = mempty & description .~ "Unauthorized access - API key missing"

response403 ∷ Response
response403 = mempty & description .~ "Forbidden - The API key does not have permission to perform the request"

geniusYieldAPISwagger ∷ Swagger
geniusYieldAPISwagger =
  toSwagger geniusYieldAPI
    & info
    . title
    .~ "Genius Yield DEX Server API"
    & info
    . version
    .~ "1.0"
    & info
    . license
    ?~ ("Apache-2.0" & url ?~ URL "https://opensource.org/licenses/apache-2-0")
      & info
      . description
    ?~ "API to interact with GeniusYield DEX."
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("tx" +> TxAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Transaction" & description ?~ "Endpoints related to transaction hex such as submitting a transaction"]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("markets" +> MarketsAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Markets" & description ?~ "Endpoints related to accessing markets information"]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("orders" +> OrdersAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Orders" & description ?~ "Endpoints related to interacting with orders"]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("settings" +> SettingsAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Settings" & description ?~ "Endpoint to get server settings such as network, version, and revision"]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("trading-fees" +> TradingFeesAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Trading Fees" & description ?~ "Endpoint to get trading fees of DEX."]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("assets" +> AssetsAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Assets" & description ?~ "Endpoint to fetch asset details."]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("order-book" +> OrderBookAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Order Book" & description ?~ "Endpoint to fetch order book."]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("historical-prices" +> "maestro" :> MaestroPriceHistoryAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Historical Prices" & description ?~ "Endpoints to fetch historical prices."]
      & applyTagsFor (subOperations (Proxy ∷ Proxy ("balances" +> BalancesAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Balances" & description ?~ "Endpoint to fetch token balances."]

geniusYieldServer ∷ Ctx → ServerT GeniusYieldAPI IO
geniusYieldServer ctx =
  ignoredAuthResult $
    handleSettings ctx
      :<|> handleOrdersApi ctx
      :<|> handleMarketsApi ctx
      :<|> handleTxApi ctx
      :<|> handleTradingFeesApi ctx
      :<|> handleAssetsApi ctx
      :<|> handleOrderBookApi ctx
      :<|> handleMaestroPriceHistoryApi ctx
      :<|> handleBalancesApi ctx
 where
  ignoredAuthResult f _authResult = f

type MainAPI =
  GeniusYieldAPI

mainAPI ∷ Proxy MainAPI
mainAPI = Proxy

mainServer ∷ Ctx → ServerT MainAPI IO
mainServer = geniusYieldServer

handleSettings ∷ Ctx → IO Settings
handleSettings ctx@Ctx {..} = do
  logInfo ctx "Settings requested."
  pure $ Settings {settingsNetwork = ctxNetworkId & customShowNetworkId, settingsVersion = showVersion PackageInfo.version, settingsRevision = gitHash, settingsBackend = "mmb"}

-- >>> customShowNetworkId GYMainnet
-- "mainnet"
-- >>> customShowNetworkId GYTestnetLegacy
-- "legacy"
-- >>> customShowNetworkId GYPrivnet
-- "privnet"
customShowNetworkId ∷ GYNetworkId → String
customShowNetworkId = show >>> removePrefix "GY" >>> removePrefix "Testnet" >>> lowerFirstChar
 where
  removePrefix ∷ String → String → String
  removePrefix pref str
    | pref `isPrefixOf` str = drop (length pref) str
    | otherwise = str
  lowerFirstChar ∷ String → String
  lowerFirstChar "" = ""
  lowerFirstChar (x : xs) = toLower x : xs

handleTradingFeesApi ∷ Ctx → IO TradingFees
handleTradingFeesApi ctx@Ctx {..} = do
  logInfo ctx "Calculating trading fees."
  (_, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft $ dexPORefs $ ctxDexInfo
  pure
    TradingFees
      { tfFlatMakerFee = fromIntegral $ pociMakerFeeFlat pocd,
        tfFlatTakerFee = fromIntegral $ pociTakerFee pocd,
        tfPercentageMakerFee = 100 * pociMakerFeeRatio pocd,
        tfPercentageTakerFee = 100 * pociMakerFeeRatio pocd
      }

handleOrderBookApi ∷ Ctx → OrderAssetPair → Maybe GYAddressBech32 → IO OrderBookInfo
handleOrderBookApi ctx@Ctx {..} orderAssetPair mownAddress = do
  logInfo ctx $ "Fetching order(s) for pair: " +|| orderAssetPair ||+ ""
  let porefs = dexPORefs ctxDexInfo
  gytime ← getCurrentGYTime
  os ← runQuery ctx $ partialOrders porefs
  let os' =
        Map.filter
          ( \PartialOrderInfo {..} →
              equivalentAssetPair (mkOrderAssetPair poiOfferedAsset poiAskedAsset) orderAssetPair
                && case mownAddress of
                  Nothing → True
                  Just ownAddress →
                    case addressToPubKeyHash $ addressFromBech32 ownAddress of
                      Nothing → True
                      Just apkh → poiOwnerKey == apkh
          )
          os
      -- Asks are sell orders.
      bids :!: asks =
        Map.foldl'
          ( \(accBids :!: accAsks) poi →
              let poi' :!: isSell = poiToOrderInfo poi orderAssetPair
               in -- If an order is offering lovelace then it is a buy order.
                  if isSell then accBids :!: poi' : accAsks else poi' : accBids :!: accAsks
                  -- Instead of inserting in lists, we could insert in a set but would need to write Ord instance...
          )
          ([] :!: [])
          os'
  pure $
    OrderBookInfo
      { obiMarketPairId = orderAssetPair,
        obiTimestamp = gytime,
        obiAsks = sortBy (\a b → compare (oiPrice a) (oiPrice b)) asks, -- sort by increasing price
        obiBids = sortBy (\a b → compare (oiPrice b) (oiPrice a)) bids -- sort by decreasing price
      }

handleBalancesApi ∷ Ctx → GYAddressBech32 → IO GYValue
handleBalancesApi ctx addr = do
  logInfo ctx $ "Fetching balance of address: " +|| addr ||+ ""
  runQuery ctx $ do
    utxos ← utxosAtAddress (addressFromBech32 addr) Nothing
    pure $ foldMapUTxOs utxoValue utxos