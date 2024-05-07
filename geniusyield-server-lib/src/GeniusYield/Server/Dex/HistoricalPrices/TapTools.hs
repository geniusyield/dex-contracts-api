module GeniusYield.Server.Dex.HistoricalPrices.TapTools (
  TapToolsPriceHistoryAPI,
  handleTapToolsPriceHistoryApi,
) where

import Fmt
import GeniusYield.Server.Ctx
import GeniusYield.Server.Dex.HistoricalPrices.TapTools.Client (TapToolsInterval, TapToolsOHLCV, TapToolsUnit (TapToolsUnit), handleTapToolsError, tapToolsOHLCV)
import GeniusYield.Server.Utils
import GeniusYield.Types
import RIO hiding (logDebug, logInfo)
import Servant

type TapToolsPriceHistoryAPI =
  Summary "Get price history using TapTools."
    :> Description "This endpoint internally calls TapTools's \"Token price OHLCV\" endpoint. Note that only the liquidity pools involving ADA and the given asset class is considered to get for aggregated price information. Price returned is in ADA."
    :> Capture "asset" GYAssetClass
    :> QueryParam' '[Required, Strict] "interval" TapToolsInterval
    :> QueryParam "numIntervals" Natural
    :> Get '[JSON] [TapToolsOHLCV]

throwNoTapToolsKeyError ∷ IO a
throwNoTapToolsKeyError = throwIO $ err500 {errBody = "No API key configured for TapTools."}

handleTapToolsPriceHistoryApi ∷ Ctx → GYAssetClass → TapToolsInterval → Maybe Natural → IO [TapToolsOHLCV]
handleTapToolsPriceHistoryApi ctx token tti mttni = do
  logInfo ctx $ "Fetching price history. Token: " +|| token ||+ ", interval: " +|| tti ||+ ""
  case ctxTapToolsProvider ctx of
    Nothing → throwNoTapToolsKeyError
    Just te → try (tapToolsOHLCV te (Just (TapToolsUnit token)) tti mttni) >>= handleTapToolsError "handleTapToolsPriceHistory"
