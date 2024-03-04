module GeniusYield.OrderBot.Adapter.Maestro.Markets where

import Control.Exception (try)
import GeniusYield.Imports (Exception, Text, throwIO)
import GeniusYield.OrderBot.Domain.Markets (MarketOHLC (..))
import GeniusYield.OrderBot.Domain.Markets qualified as Domain
import GeniusYield.OrderBot.Types (DexPair (..), TokenDisplayDetails (..), adaTokenDisplayDetails)
import GeniusYield.Providers.Common (silenceHeadersClientError)
import GeniusYield.Types (GYAssetClass (..), parseAssetClassWithSep)
import Maestro.Client.V1
import Maestro.Types.V1
import RIO.List (find)
import RIO.Text (pack)

data MaestroMarketsException
  = MmeDeserializationException !Text
  | MmeRequestException !Text !MaestroError
  | MmeRequiredRegistryDetailsNotFound !GYAssetClass
  deriving stock (Show)
  deriving anyclass (Exception)

data MaestroMarketsProvider = MaestroMarketsProvider !(MaestroEnv 'V1) !Dex

-- | Remove headers (if `MaestroError` contains `ClientError`).
silenceHeadersMaestroClientError ∷ MaestroError → MaestroError
silenceHeadersMaestroClientError (ServantClientError e) = ServantClientError $ silenceHeadersClientError e
silenceHeadersMaestroClientError other = other

throwMmeApiError ∷ Text → MaestroError → IO a
throwMmeApiError locationInfo =
  throwIO . MmeRequestException locationInfo . silenceHeadersMaestroClientError

-- | Utility function to handle Maestro errors, which also removes header (if present) so as to conceal API key.
handleMaestroError ∷ Text → Either MaestroError a → IO a
handleMaestroError locationInfo = either (throwMmeApiError locationInfo) pure

instance Domain.Markets MaestroMarketsProvider where
  getMarkets (MaestroMarketsProvider menv _) = do
    -- TODO: This must be cached...
    DexPairResponse {dexPairResponsePairs} ← try (pairsFromDex menv GeniusYield) >>= handleMaestroError (locationInfoPrefix <> "fetching market pairs")
    traverse fromDexPairInfo dexPairResponsePairs
   where
    locationInfoPrefix = "getMarkets: "

    fromDexPairInfo DexPairInfo {..} = do
      a' ← getTokenDetails dexPairInfoCoinAPolicy dexPairInfoCoinAAssetName
      b' ← getTokenDetails dexPairInfoCoinBPolicy dexPairInfoCoinBAssetName
      pure $
        DexPair
          { dpCurrencyToken = a',
            dpCommodityToken = b',
            dpMarketPairId = dexPairInfoPair
          }
    getTokenDetails policyId tokenName = do
      tokenAC ← getAssetClass policyId tokenName
      a ← if tokenAC == GYLovelace then pure $ Left adaTokenDisplayDetails else try (Right . getTimestampedData <$> assetInfo menv (NonAdaNativeToken policyId tokenName)) >>= handleMaestroError (locationInfoPrefix <> "fetching particular token details")
      case a of
        Left a' → pure a'
        Right AssetInfo {assetInfoTokenRegistryMetadata = tdata} →
          case tdata of
            Just TokenRegistryMetadata {tokenRegistryMetadataTicker = Just ticker, tokenRegistryMetadataDecimals = Just decimals} → pure $ TokenDisplayDetails ticker decimals tokenAC
            _ → throwIO $ MmeRequiredRegistryDetailsNotFound tokenAC

    getAssetClass (PolicyId polId) (TokenName tkName) =
      if polId == mempty
        then pure GYLovelace
        else case parseAssetClassWithSep '.' (polId <> "." <> tkName) of
          Left e → throwIO $ MmeDeserializationException (pack e)
          Right ac → pure ac

  -- TODO: Handle exceptions
  getHistoricalPrices (MaestroMarketsProvider menv dex) marketPair mres mfromDate mtoDate mlimit msort = fmap fromMaestroOHLC <$> pricesFromDex menv dex (TaggedText marketPair) (toMaestroResolution <$> mres) mfromDate mtoDate mlimit (toMaestroSort <$> msort)
   where
    toMaestroSort Domain.Asc = Ascending
    toMaestroSort Domain.Desc = Descending
    fromMaestroOHLC OHLCCandleInfo {..} =
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
          marketOHLCTimestamp = ohlcCandleInfoTimestamp
        }

-- >>> toMaestroResolution Domain.Res1m
-- 1m
-- >>> toMaestroResolution Domain.Res1mo
-- 1mo
toMaestroResolution ∷ Domain.Resolution → Resolution
toMaestroResolution mr =
  let mr' = show mr
      findResult = find (\maestroRes → show maestroRes == mr') [minBound ∷ Resolution .. maxBound]
   in case findResult of
        Just res → res
        Nothing → error $ "toMaestroResolution: absurd, resolution doesn't map to Maestro's: " <> mr'

-- FIXME: Delete below comments.
-- toMaestroResolution Res1m = MaestroResolution1m
-- toMaestroResolution Res5m = MaestroResolution5m
-- toMaestroResolution Res15m = MaestroResolution15m
-- toMaestroResolution Res30m = MaestroResolution30m
-- toMaestroResolution Res1h = MaestroResolution1h
