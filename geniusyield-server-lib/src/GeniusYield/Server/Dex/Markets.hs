module GeniusYield.Server.Dex.Markets (
  MarketsAPI,
  handleMarketsApi,
) where

import Data.Aeson (camelTo2)
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import GeniusYield.OrderBot.Domain.Markets (HasMarkets (getMarkets))
import GeniusYield.OrderBot.Types (OrderAssetPair (commodityAsset, currencyAsset))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils (addSwaggerDescription, logInfo)
import GeniusYield.Types
import RIO hiding (logDebug, logInfo)
import Servant

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import           Data.Proxy
>>> import qualified Data.Swagger               as Swagger
-}

data Market = Market
  { marketId ∷ !OrderAssetPair,
    baseAsset ∷ !GYAssetClass,
    targetAsset ∷ !GYAssetClass
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] Market

{- |
>>> Aeson.encode (Swagger.toSchema (Proxy :: Proxy Market))
"{\"description\":\"Market information\",\"required\":[\"market_id\",\"base_asset\",\"target_asset\"],\"properties\":{\"market_id\":{\"$ref\":\"#/definitions/MarketId\"},\"base_asset\":{\"$ref\":\"#/definitions/BaseAsset\"},\"target_asset\":{\"$ref\":\"#/definitions/TargetAsset\"}},\"type\":\"object\"}"
-}
instance Swagger.ToSchema Market where
  declareNamedSchema = do
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = camelTo2 '_'}
      & addSwaggerDescription "Market information"

type MarketsAPI =
  Summary "Get markets information for the DEX."
    :> Description "Returns the list of markets information supported by GeniusYield DEX."
    :> Get '[JSON] [Market]

handleMarketsApi ∷ Ctx → ServerT MarketsAPI IO
handleMarketsApi = handleMarkets

handleMarkets ∷ Ctx → IO [Market]
handleMarkets ctx = do
  logInfo ctx "Fetching markets."
  fmap fromOrderAssetPair <$> getMarkets (ctxMaestroProvider ctx)
 where
  fromOrderAssetPair ∷ OrderAssetPair → Market
  fromOrderAssetPair oap = Market {marketId = oap, baseAsset = currencyAsset oap, targetAsset = commodityAsset oap}