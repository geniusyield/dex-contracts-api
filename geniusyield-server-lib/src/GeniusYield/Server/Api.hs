module GeniusYield.Server.Api (
  GeniusYieldAPI,
  GYBalance (..),
  geniusYieldAPI,
  geniusYieldServer,
  MainAPI,
  mainAPI,
  mainServer,
  geniusYieldAPISwagger,
) where

import Control.Lens ((?~))
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as K
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Strict qualified as Strict
import Data.Strict.Tuple
import Data.Swagger
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Version (showVersion)
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..), PartialOrderInfo (..), partialOrders)
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.OrderBot.Domain.Markets
import GeniusYield.Scripts (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Assets
import GeniusYield.Server.Auth (APIKeyAuthProtect, V0)
import GeniusYield.Server.Constants (gitHash)
import GeniusYield.Server.Ctx
import GeniusYield.Server.Dex.HistoricalPrices.Maestro
import GeniusYield.Server.Dex.Markets (MarketsAPI, handleMarketsApi)
import GeniusYield.Server.Dex.PartialOrder (OrderInfo (..), OrdersAPI, handleOrdersApi, poiToOrderInfo)
import GeniusYield.Server.Tx (TxAPI, handleTxApi)
import GeniusYield.Server.Utils
import GeniusYield.TxBuilder (GYTxQueryMonad (utxosAtAddress))
import GeniusYield.Types
import PackageInfo_geniusyield_server_lib qualified as PackageInfo
import RIO hiding (asks, logDebug, logInfo)
import RIO.Char (toLower)
import RIO.List (isPrefixOf)
import RIO.Map qualified as Map
import RIO.Text qualified as T
import Servant
import Servant.Swagger

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import           Data.Proxy
>>> import qualified Data.Swagger               as Swagger
>>> import GeniusYield.Types
-}

-------------------------------------------------------------------------------
-- Settings.
-------------------------------------------------------------------------------

type SettingsPrefix ∷ Symbol
type SettingsPrefix = "settings"

data Settings = Settings
  { settingsNetwork ∷ !String,
    settingsVersion ∷ !String,
    settingsRevision ∷ !String,
    settingsBackend ∷ !String,
    settingsAddress ∷ !(Maybe GYAddressBech32),
    settingsStakeAddress ∷ !(Maybe GYStakeAddressBech32),
    settingsCollateral ∷ !(Maybe GYTxOutRef)
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
-- Balances.
-------------------------------------------------------------------------------

newtype GYBalance = GYBalance {unGYBalance ∷ GYValue}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

{- |

>>> LBS8.putStrLn . Aeson.encode . GYBalance . valueFromList $ [(GYLovelace,22),(GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",101)]
{"lovelace":"22","ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44":"101"}
-}
instance Aeson.ToJSON GYBalance where
  toJSON = Aeson.object . map (RIO.uncurry assetPairToKVT) . valueToList . unGYBalance
  toEncoding = Aeson.pairs . foldMap (RIO.uncurry assetPairToKVT) . valueToList . unGYBalance

assetPairToKVT ∷ Aeson.KeyValue kv ⇒ GYAssetClass → Integer → kv
assetPairToKVT ac i = K.fromText (f ac) Aeson..= toUrlPiece i
 where
  f GYLovelace = "lovelace"
  f (GYToken cs tk) = mintingPolicyIdToText cs <> T.cons '.' (tokenNameToHex tk)

instance Swagger.ToSchema GYBalance where
  declareNamedSchema _ = do
    pure $
      Swagger.named "GYBalance" $
        mempty
          & Swagger.type_
            ?~ Swagger.SwaggerObject
          & Swagger.example
            ?~ toJSON
              ( GYBalance $
                  valueFromList
                    [ (GYLovelace, 22),
                      (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD", 101)
                    ]
              )
          & Swagger.description
            ?~ "A multi asset quantity, represented as map where each key represents an asset: policy ID and token name in hex concatenated by a dot."

-------------------------------------------------------------------------------
-- Server's API.
-------------------------------------------------------------------------------

type SettingsAPI = Summary "Server settings" :> Description "Get server settings such as network, version, and revision. Optionally if a collateral UTxO reference and signing key are individually configured in the server (provided server is spun up locally), then it's details are also returned." :> Get '[JSON] Settings

type TradingFeesAPI =
  Summary "Trading fees"
    :> Description "Get trading fees of DEX."
    :> Get '[JSON] TradingFees

type OrderBookAPI = Summary "Order book" :> Description "Get order book for a specific market." :> Capture "market-id" OrderAssetPair :> QueryParam "address" GYAddressBech32 :> Get '[JSON] OrderBookInfo

type BalancesAPI = Summary "Balances" :> Description "Get token balances of an address." :> Capture "address" GYAddressBech32 :> Get '[JSON] GYBalance

type V0API =
  "settings" :> SettingsAPI
    :<|> "orders" :> OrdersAPI
    :<|> "markets" :> MarketsAPI
    :<|> "tx" :> TxAPI
    :<|> "trading-fees" :> TradingFeesAPI
    :<|> "assets" :> AssetsAPI
    :<|> "order-books" :> OrderBookAPI
    :<|> "historical-prices" :> "maestro" :> MaestroPriceHistoryAPI
    :<|> "balances" :> BalancesAPI

type GeniusYieldAPI = APIKeyAuthProtect :> V0 :> V0API

geniusYieldAPI ∷ Proxy GeniusYieldAPI
geniusYieldAPI = Proxy

infixr 4 +>

type family (+>) (api1 ∷ k) (api2 ∷ Type) where
  (+>) api1 api2 = APIKeyAuthProtect :> V0 :> api1 :> api2

geniusYieldAPISwagger ∷ Swagger
geniusYieldAPISwagger =
  toSwagger geniusYieldAPI
    & info
      . title
      .~ "GeniusYield DEX Server API"
    & info
      . version
      .~ "0.0.1"
    & info
      . license
      ?~ ("Apache-2.0" & url ?~ URL "https://opensource.org/licenses/apache-2-0")
    & info
      . contact
      ?~ ( mempty
            & url
              ?~ URL "https://www.geniusyield.co/"
            & email
              ?~ "support@geniusyield.co"
            & name
              ?~ "GeniusYield Technical Support"
         )
    & info
      . description
      ?~ "API to interact with GeniusYield DEX."
    & applyTagsFor (subOperations (Proxy ∷ Proxy ("tx" +> TxAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Transaction" & description ?~ "Endpoints related to transaction hex such as submitting a transaction"]
    & applyTagsFor (subOperations (Proxy ∷ Proxy ("markets" +> MarketsAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Markets" & description ?~ "Endpoints related to accessing markets information"]
    & applyTagsFor (subOperations (Proxy ∷ Proxy ("orders" +> OrdersAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Orders" & description ?~ "Endpoints related to interacting with orders"]
    & applyTagsFor (subOperations (Proxy ∷ Proxy ("settings" +> SettingsAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Settings" & description ?~ "Endpoint to get server settings such as network, version, and revision"]
    & applyTagsFor (subOperations (Proxy ∷ Proxy ("trading-fees" +> TradingFeesAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Trading Fees" & description ?~ "Endpoint to get trading fees of DEX."]
    & applyTagsFor (subOperations (Proxy ∷ Proxy ("assets" +> AssetsAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Assets" & description ?~ "Endpoint to fetch asset details."]
    & applyTagsFor (subOperations (Proxy ∷ Proxy ("order-books" +> OrderBookAPI)) (Proxy ∷ Proxy GeniusYieldAPI)) ["Order Book" & description ?~ "Endpoint to fetch order book."]
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
  pure $ Settings {settingsNetwork = ctxNetworkId & customShowNetworkId, settingsVersion = showVersion PackageInfo.version, settingsRevision = gitHash, settingsBackend = "mmb", settingsAddress = fmap (addressToBech32 . Strict.snd) ctxSigningKey, settingsStakeAddress = ctxStakeAddress, settingsCollateral = ctxCollateral}

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
  (_, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft $ dexPORefs ctxDexInfo
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

handleBalancesApi ∷ Ctx → GYAddressBech32 → IO GYBalance
handleBalancesApi ctx addr = do
  logInfo ctx $ "Fetching balance of address: " +|| addr ||+ ""
  runQuery ctx $ do
    utxos ← utxosAtAddress (addressFromBech32 addr) Nothing
    pure $ GYBalance $ foldMapUTxOs utxoValue utxos
