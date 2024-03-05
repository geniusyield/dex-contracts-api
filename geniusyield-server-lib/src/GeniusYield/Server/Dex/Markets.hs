module GeniusYield.Server.Dex.Markets (
  MarketsAPI,
  handleMarketsApi,
) where

import Data.Aeson (camelTo2, (.=))
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..))
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.HTTP.Errors (
  GYApiError (..),
  IsGYApiError (..),
 )
import GeniusYield.Imports
import GeniusYield.OrderBot.Domain.Markets (Markets (getMarkets))
import GeniusYield.OrderBot.Types (DexPair (..), OrderAssetPair (commodityAsset, currencyAsset), TokenDisplayDetails (..), mkOrderAssetPair)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils (addSwaggerDescription, addSwaggerExample, dropAndCamelToSnake, logInfo)
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import Network.HTTP.Types (status400)
import RIO (Word64)
import Servant

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import           Data.Proxy
>>> import qualified Data.Swagger               as Swagger
-}

-- TODO: Verify it's tojson instance.
-- >>> Aeson.encode (MarketId "123")
-- "\"123\""
newtype MarketId = MarketId Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

{- |
>>> Aeson.encode (Swagger.toSchema (Proxy :: Proxy MarketId))
"{\"description\":\"Identifier of a ticker with delimiter to separate base/target.\",\"example\":\"ADA/GENS\",\"type\":\"string\"}"
-}
instance Swagger.ToSchema MarketId where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Identifier of a ticker with delimiter to separate base/target."
      & addSwaggerExample "ADA/GENS"

newtype BaseAsset = BaseAsset Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

instance Swagger.ToSchema BaseAsset where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Ticker of a the base cryptoasset"
      & addSwaggerExample "ADA"

newtype TargetAsset = TargetAsset Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

instance Swagger.ToSchema TargetAsset where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Ticker of a the target cryptoasset"
      & addSwaggerExample "GENS"

data Market = Market
  { marketId ∷ !OrderAssetPair,
    baseAsset ∷ !GYAssetClass,
    targetAsset ∷ !GYAssetClass,
    baseAssetTicker ∷ !(Maybe Text),
    targetAssetTicker ∷ !(Maybe Text),
    baseAssetDecimals ∷ !(Maybe Word64),
    targetAssetDecimals ∷ !(Maybe Word64)
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
  "markets"
    :> Get '[JSON] [Market]

handleMarketsApi ∷ Ctx → ServerT MarketsAPI IO
handleMarketsApi ctx = handleMarkets ctx

handleMarkets ∷ Ctx → IO [Market]
handleMarkets ctx = do
  logInfo ctx "Fetching markets."
  fmap fromDexPair <$> getMarkets (ctxMarketsProvider ctx)
 where
  -- We assume following for now as there is no link b/w display name and actual asset name, for instance @LENFI@ token has asset name @AADA@.
  fromDexPair ∷ DexPair → Market
  fromDexPair DexPair {..} =
    let baseAsset' = tddAssetClass dpCurrencyToken
        targetAsset' = tddAssetClass dpCommodityToken
     in Market {marketId = mkOrderAssetPair baseAsset' targetAsset', baseAsset = baseAsset', targetAsset = targetAsset', baseAssetTicker = Just $ tddTicker dpCurrencyToken, targetAssetTicker = Just $ tddTicker dpCommodityToken, baseAssetDecimals = Just $ tddDecimals dpCurrencyToken, targetAssetDecimals = Just $ tddDecimals dpCommodityToken}

-- Market
--   { marketId = MarketId dexMarketPairId,
--     baseAsset = if currencyAsset dexOrderAssetPair == GYLovelace then BaseAsset "ADA" else throwIO $ userError "Base asset is not ADA",
--     targetAsset = TargetAsset $ commodityAsset dexOrderAssetPair
--   }

--   (_, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft $ dexPORefs $ ctxDexInfo
--   pure
--     TradingFees
--       { tfFlatMakerFee = fromIntegral $ pociMakerFeeFlat pocd,
--         tfFlatTakerFee = fromIntegral $ pociTakerFee pocd,
--         tfPercentageMakerFee = 100 * pociMakerFeeRatio pocd,
--         tfPercentageTakerFee = 100 * pociMakerFeeRatio pocd
--       }
