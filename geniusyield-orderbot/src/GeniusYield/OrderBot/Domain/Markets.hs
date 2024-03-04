module GeniusYield.OrderBot.Domain.Markets (
  Resolution (..),
  Sort (..),
  MarketOHLC (..),
  Markets (..),
) where

import Deriving.Aeson
import GeniusYield.OrderBot.Types (DexPair)
import RIO (Data, Natural, Word64, toConstr, (>>>))
import RIO.List (find)
import RIO.Text (Text, pack)
import RIO.Time (Day, UTCTime)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

data Resolution = Res1m | Res5m | Res15m | Res30m | Res1h | Res4h | Res1d | Res1w | Res1mo
  deriving stock (Eq, Generic, Data, Enum, Bounded)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Res"]] Resolution

-- >>> show Res1mo
-- "1mo"
instance Show Resolution where
  show = toConstr >>> show >>> drop 3

instance ToHttpApiData Resolution where
  toQueryParam = pack . show

-- >>> parseQueryParam "1mo" :: Either Text Resolution
-- Right 1mo
-- >>> parseQueryParam "1m" :: Either Text Resolution
-- Right 1m
-- >>> parseQueryParam "Res1m" :: Either Text Resolution
-- Left "invalid resolution"
instance FromHttpApiData Resolution where
  parseQueryParam t =
    maybe (Left "invalid resolution") pure (find ((==) t . toQueryParam) [minBound ∷ Resolution .. maxBound])

data Sort = Asc | Desc

type FromDate = Day

type ToDate = Day

-- Is choice of `Double` here appropriate?
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
    marketOHLCTimestamp ∷ !UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "marketOHLC", CamelToSnake]] MarketOHLC

-- TODO: Change from List?
class Markets a where
  getMarkets ∷ a → IO [DexPair]
  getHistoricalPrices ∷ a → Text → Maybe Resolution → Maybe FromDate → Maybe ToDate → Maybe Word64 → Maybe Sort → IO [MarketOHLC]
