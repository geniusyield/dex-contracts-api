module GeniusYield.Server.Dex.HistoricalPrices.Maestro (
  mkLimit,
  unLimit,
  Limit,
  MaestroPriceHistoryAPI,
  handleMaestroPriceHistoryApi,
) where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Swagger qualified as Swagger
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Deriving.Aeson
import Fmt
import GHC.TypeLits (AppendSymbol, Symbol)
import GeniusYield.OrderBot.Adapter.Maestro (MaestroProvider (..), handleMaestroError)
import GeniusYield.OrderBot.Domain.Assets (AssetDetails (adAssetTicker), AssetTicker (..))
import GeniusYield.OrderBot.Domain.Markets (OrderAssetPair (..))
import GeniusYield.Server.Assets (handleAssetsApi)
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils
import GeniusYield.Types
import Maestro.Client.V1 (pricesFromDex)
import Maestro.Types.V1 (Dex, OHLCCandleInfo (..), Order, Resolution, TaggedText (TaggedText))
import RIO hiding (logDebug, logInfo)
import RIO.Text (unpack)
import RIO.Time (Day)
import Servant

type MarketOHLCPrefix ∷ Symbol
type MarketOHLCPrefix = "marketOHLC"

data MarketOHLC = MarketOHLC
  { marketOHLCBaseClose ∷ !Double,
    marketOHLCBaseHigh ∷ !Double,
    marketOHLCBaseLow ∷ !Double,
    marketOHLCBaseOpen ∷ !Double,
    marketOHLCBaseVolume ∷ !Double,
    marketOHLCTargetClose ∷ !Double,
    marketOHLCTargetHigh ∷ !Double,
    marketOHLCTargetLow ∷ !Double,
    marketOHLCTargetOpen ∷ !Double,
    marketOHLCTargetVolume ∷ !Double,
    marketOHLCCount ∷ !Natural,
    marketOHLCTimestamp ∷ !GYTime
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix MarketOHLCPrefix, CamelToSnake]] MarketOHLC

instance Swagger.ToSchema MarketOHLC where
  declareNamedSchema =
    let baseD = 0.207742
        targetD = 4.813658
     in Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @MarketOHLCPrefix}
          & addSwaggerDescription "Returns market activity in candlestick OHLC format for a specific DEX and token pair"
          & addSwaggerExample (toJSON $ MarketOHLC {marketOHLCBaseClose = baseD, marketOHLCBaseHigh = baseD, marketOHLCBaseLow = baseD, marketOHLCBaseOpen = baseD, marketOHLCBaseVolume = 25.21128, marketOHLCTargetClose = targetD, marketOHLCTargetHigh = targetD, marketOHLCTargetLow = targetD, marketOHLCTargetOpen = targetD, marketOHLCTargetVolume = 121.358488, marketOHLCCount = 1, marketOHLCTimestamp = "2024-03-07T23:45:00Z"})

newtype Limit = Limit {unLimit ∷ Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral, ToHttpApiData, ToJSON)

mkLimit ∷ Word64 → Maybe Limit
mkLimit n = if n >= 1 && n <= 50_000 then Just (Limit n) else Nothing

limitFromWord64FailMessage ∷ Text
limitFromWord64FailMessage = "Limit must be between 1 and 50,000"

instance FromJSON Limit where
  parseJSON v = do
    w ← parseJSON v
    case mkLimit w of
      Just l → pure l
      Nothing → fail $ unpack limitFromWord64FailMessage

instance FromHttpApiData Limit where
  parseQueryParam t = do
    w ← parseQueryParam t
    case mkLimit w of
      Just l → Right l
      Nothing → Left limitFromWord64FailMessage

instance Swagger.ToParamSchema Limit where
  toParamSchema _ = mempty & Swagger.type_ ?~ Swagger.SwaggerInteger & Swagger.minimum_ ?~ 1 & Swagger.maximum_ ?~ 50_000

instance Swagger.ToSchema Limit where
  declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Limit") $ Swagger.paramSchemaToSchema p & Swagger.example ?~ toJSON (Limit 1)

newtype MaestroOrder = MaestroOrder {unMaestroOrder ∷ Order}
  deriving stock (Show)
  deriving newtype (ToHttpApiData, FromHttpApiData, Enum, Bounded, ToJSON)

instance Swagger.ToParamSchema MaestroOrder where
  toParamSchema = commonEnumParamSchemaRecipe

newtype MaestroResolution = MaestroResolution {unMaestroResolution ∷ Resolution}
  deriving stock (Show)
  deriving newtype (ToHttpApiData, FromHttpApiData, FromJSON, ToJSON, Enum, Bounded)

instance Swagger.ToParamSchema MaestroResolution where
  toParamSchema = commonEnumParamSchemaRecipe

newtype MaestroDex = MaestroDex {unMaestroDex ∷ Dex}
  deriving stock (Show)
  deriving newtype (ToHttpApiData, FromHttpApiData, FromJSON, ToJSON, Enum, Bounded)

instance Swagger.ToParamSchema MaestroDex where
  toParamSchema = commonEnumParamSchemaRecipe

type MaestroPriceHistoryAPI =
  Summary "Get price history using Maestro."
    :> Description ("This endpoint internally calls Maestro's \"DEX And Pair OHLC\" endpoint. " `AppendSymbol` CommonMaestroKeyRequirementText)
    :> Capture "market-id" OrderAssetPair
    :> Capture "dex" MaestroDex
    :> QueryParam "resolution" MaestroResolution
    :> QueryParam "from" Day
    :> QueryParam "to" Day
    :> QueryParam "limit" Limit
    :> QueryParam "sort" MaestroOrder
    :> Get '[JSON] [MarketOHLC]

handleMaestroPriceHistoryApi ∷ Ctx → ServerT MaestroPriceHistoryAPI IO
handleMaestroPriceHistoryApi = handleMaestroPriceHistory

handleMaestroPriceHistory ∷ Ctx → OrderAssetPair → MaestroDex → Maybe MaestroResolution → Maybe Day → Maybe Day → Maybe Limit → Maybe MaestroOrder → IO [MarketOHLC]
handleMaestroPriceHistory ctx marketId (unMaestroDex → dex) (fmap unMaestroResolution → mresolution) mfrom mto mlimit (fmap unMaestroOrder → msort) = do
  logInfo ctx $ "Fetching price history. Market: " +|| marketId ||+ ", DEX: " +|| dex ||+ ", Resolution: " +|| mresolution ||+ ", From: " +|| mfrom ||+ ", To: " +|| mto ||+ ", Limit: " +|| mlimit ||+ ", Sort: " +|| msort ||+ ""
  let MaestroProvider menv = ctxMaestroProvider ctx
  currencyTicker ← adAssetTicker <$> handleAssetsApi ctx (currencyAsset marketId)
  commodityTicker ← adAssetTicker <$> handleAssetsApi ctx (commodityAsset marketId)
  case (currencyTicker, commodityTicker) of
    (Just (AssetTicker curTicker), Just (AssetTicker comTicker)) → do
      maestroOhlcList ← try (pricesFromDex menv dex (TaggedText $ curTicker <> "-" <> comTicker) mresolution mfrom mto (unLimit <$> mlimit) msort) >>= handleMaestroError "handleMaestroPriceHistory"
      pure $ fromMaestroOhlc <$> maestroOhlcList
    _anyOther → do
      throwIO $ err400 {errBody = "Couldn't find ticker for currency or commodity asset."}
 where
  fromMaestroOhlc OHLCCandleInfo {..} =
    MarketOHLC
      { marketOHLCBaseClose = ohlcCandleInfoCoinAClose,
        marketOHLCBaseHigh = ohlcCandleInfoCoinAHigh,
        marketOHLCBaseLow = ohlcCandleInfoCoinALow,
        marketOHLCBaseOpen = ohlcCandleInfoCoinAOpen,
        marketOHLCBaseVolume = ohlcCandleInfoCoinAVolume,
        marketOHLCTargetClose = ohlcCandleInfoCoinBClose,
        marketOHLCTargetHigh = ohlcCandleInfoCoinBHigh,
        marketOHLCTargetLow = ohlcCandleInfoCoinBLow,
        marketOHLCTargetOpen = ohlcCandleInfoCoinBOpen,
        marketOHLCTargetVolume = ohlcCandleInfoCoinBVolume,
        marketOHLCCount = ohlcCandleInfoCount,
        marketOHLCTimestamp = ohlcCandleInfoTimestamp & utcTimeToPOSIXSeconds & timeFromPOSIX
      }
