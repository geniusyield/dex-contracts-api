module GeniusYield.Server.Dex.HistoricalPrices.TapTools (
  TapToolsPriceHistoryAPI,
  handleTapToolsPriceHistoryApi,
) where

import Control.Lens ((?~))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Fmt
import GeniusYield.Server.Ctx
import GeniusYield.Server.Dex.HistoricalPrices.TapTools.Client (TapToolsInterval, TapToolsOHLCV, TapToolsUnit (TapToolsUnit), handleTapToolsError, tapToolsOHLCV)
import GeniusYield.Server.Utils
import GeniusYield.Types
import RIO hiding (logDebug, logInfo)
import Servant

newtype TapToolsNumIntervals = TapToolsNumIntervals {unTapToolsNumIntervals ∷ Natural}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromHttpApiData, Swagger.ToParamSchema)

-- Since this is a query parameter, our schema description wouldn't be registered for in swagger specification :(. Following OpenAPI 3.0 would allow for it.
instance Swagger.ToSchema TapToolsNumIntervals where
  declareNamedSchema p =
    pure
      $ Swagger.named "TapToolsNumIntervals"
      $ Swagger.paramSchemaToSchema p
      & Swagger.description
      ?~ "The number of intervals to return, e.g. if you want 180 days of data in 1d intervals, then pass 180 here."

type TapToolsPriceHistoryAPI =
  Summary "Get price history using TapTools."
    :> Description "This endpoint internally calls TapTools's \"Token price OHLCV\" endpoint. Note that only the liquidity pools involving ADA and the given asset class is considered to get for aggregated price information. Price returned is in ADA."
    :> Capture "asset" GYAssetClass
    :> QueryParam' '[Required, Strict] "interval" TapToolsInterval
    :> QueryParam "numIntervals" TapToolsNumIntervals
    :> Get '[JSON] [TapToolsOHLCV]

throwNoTapToolsKeyError ∷ IO a
throwNoTapToolsKeyError = throwIO $ err500 {errBody = "No API key configured for TapTools."}

handleTapToolsPriceHistoryApi ∷ Ctx → GYAssetClass → TapToolsInterval → Maybe TapToolsNumIntervals → IO [TapToolsOHLCV]
handleTapToolsPriceHistoryApi ctx token tti (fmap unTapToolsNumIntervals → mttni) = do
  logInfo ctx $ "Fetching price history. Token: " +|| token ||+ ", interval: " +|| tti ||+ ""
  case ctxTapToolsProvider ctx of
    Nothing → throwNoTapToolsKeyError
    Just te → try (tapToolsOHLCV te (Just (TapToolsUnit token)) tti mttni) >>= handleTapToolsError "handleTapToolsPriceHistory"
