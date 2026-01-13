{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GeniusYield.Api.Oracle (
  -- * Core types
  Price (..),
  PriceIndicator (..),
  OracleCertificate (..),
  OracleQuote (..),
  FreshnessConstraints (..),
  noFreshnessConstraints,
  freshnessConstraints,
  freshnessConstraintsSeconds,

  -- * Signing helpers
  mkOracleCertificate,

  -- * Aggregator configuration
  OracleCfg (..),
  defaultOracleCfg,

  -- * Providers & aggregators
  PriceProvider (..),
  OracleAggregator,
  mkAggregator,
  mkAggregatorWithClock,

  -- * Estimation
  oracleEstimate,
  oracleEstimateUnbounded,

  -- * Testing helpers
  newMockProvider,
  SignatureOffchain (..),
) where

import Cardano.Api qualified as Api
import Codec.Serialise (serialise)
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Crypto
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GeniusYield.Crypto (SignatureOffchain (..))
import GeniusYield.Imports
import GeniusYield.Types (GYAssetClass (..), GYPaymentSigningKey, assetClassToPlutus, paymentSigningKeyToApi)
import PlutusLedgerApi.V1 (Data (..))
import PlutusLedgerApi.V1.Value (AssetClass (..), CurrencySymbol (..), TokenName (..))
import PlutusTx qualified
import PlutusTx.Builtins (fromBuiltin)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Minimal price wrapper for clarity.
newtype Price = Price {getPrice :: Rational}
  deriving stock (Eq, Generic, Show, Ord)

-- | Signals about the state of aggregation.
data PriceIndicator
  = PriceMismatch1
  | PriceMismatch2
  | PriceUnavailable
  | PriceSourceFail [String] Price
  | PriceAverage Price
  deriving stock (Eq, Generic, Show)

-- | Signed oracle certificate (price + timestamp + signature).
data OracleCertificate = OracleCertificate
  { ocPrice :: !Price,
    ocBaseAsset :: !GYAssetClass,
    ocQuoteAsset :: !GYAssetClass,
    ocTimestamp :: !UTCTime,
    ocSignature :: !SignatureOffchain
  }
  deriving stock (Eq, Generic, Show)

-- | Aggregated oracle quote paired with indicator metadata.
data OracleQuote = OracleQuote
  { oqIndicator :: !PriceIndicator,
    oqCertificate :: !(Maybe OracleCertificate)
  }
  deriving stock (Eq, Generic, Show)

mkOracleCertificate
  :: GYPaymentSigningKey
  -> GYAssetClass
  -> GYAssetClass
  -> Rational
  -> UTCTime
  -> OracleCertificate
mkOracleCertificate sk baseAsset quoteAsset price timestamp =
  OracleCertificate
    { ocPrice = Price price,
      ocBaseAsset = baseAsset,
      ocQuoteAsset = quoteAsset,
      ocTimestamp = timestamp,
      ocSignature = SignatureOffchain (BA.convert signature)
    }
 where
  timestampMs = floor (utcTimeToPOSIXSeconds timestamp * 1000)
  messageBytes =
    LBS.toStrict (serialise $ priceTimestampData baseAsset quoteAsset price timestampMs)
  secretBytes = Api.serialiseToRawBytes $ paymentSigningKeyToApi sk
  signature = case Crypto.secretKey secretBytes of
    CryptoFailed err -> error $ "Invalid oracle signing key: " <> show err
    CryptoPassed secretKey -> Crypto.sign secretKey (Crypto.toPublic secretKey) messageBytes

-- | Constraints applied when reusing cached certificates.
data FreshnessConstraints = FreshnessConstraints
  { fcValidFor :: !NominalDiffTime,
    fcSigningSlack :: !NominalDiffTime
  }
  deriving stock (Eq, Generic, Show)

-- | No freshness constraints (cache entries are always acceptable).
noFreshnessConstraints :: FreshnessConstraints
noFreshnessConstraints = FreshnessConstraints 0 0

-- | Construct freshness constraints from @NominalDiffTime@ values.
freshnessConstraints :: NominalDiffTime -> NominalDiffTime -> FreshnessConstraints
freshnessConstraints = FreshnessConstraints

-- | Convenience helper to construct constraints from seconds.
freshnessConstraintsSeconds :: Integer -> Integer -> FreshnessConstraints
freshnessConstraintsSeconds valid slack =
  FreshnessConstraints (secs valid) (secs slack)
 where
  secs :: Integer -> NominalDiffTime
  secs = secondsToNominalDiffTime . fromIntegral . max 0

-- | Aggregator configuration.
data OracleCfg = OracleCfg
  { ocThreshold1 :: !Double,
    ocThreshold2 :: !Double,
    ocCacheDuration :: !NominalDiffTime
  }
  deriving stock (Eq, Generic, Show)

-- | Default aggregator configuration.
defaultOracleCfg :: OracleCfg
defaultOracleCfg =
  OracleCfg
    { ocThreshold1 = 0.3,
      ocThreshold2 = 0.6,
      ocCacheDuration = secondsToNominalDiffTime 300
    }

-- | A simple provider interface (IO for now; can generalise later).
data PriceProvider = PriceProvider
  { ppName :: !String,
    ppGet :: !(GYAssetClass -> GYAssetClass -> IO (Either String OracleCertificate))
  }

type CacheKey = (GYAssetClass, GYAssetClass)

type CacheEntry = (UTCTime, OracleQuote)

-- | Oracle aggregator with cache.
data OracleAggregator = OA
  { oaCfg :: !OracleCfg,
    oaProv :: !(NonEmpty PriceProvider),
    oaWeights :: !(NonEmpty Int),
    oaCache :: !(MVar (Map.Map CacheKey CacheEntry)),
    oaClock :: !(IO UTCTime)
  }

-- | Construct an aggregator using the real clock.
mkAggregator :: OracleCfg -> [PriceProvider] -> [Int] -> IO OracleAggregator
mkAggregator cfg = mkAggregatorWithClock cfg getCurrentTime

-- | Construct an aggregator with a custom clock (useful for tests).
mkAggregatorWithClock :: OracleCfg -> IO UTCTime -> [PriceProvider] -> [Int] -> IO OracleAggregator
mkAggregatorWithClock cfg clock ps ws = case (NE.nonEmpty ps, NE.nonEmpty ws) of
  (Just ps', Just ws') | length ps == length ws -> do
    ttl <- resolveCacheDuration (ocCacheDuration cfg)
    cacheVar <- newMVar Map.empty
    let cfg' = cfg {ocCacheDuration = ttl}
    pure $ OA cfg' ps' ws' cacheVar clock
  _ -> error "mkAggregator: providers and weights must be non-empty and same length"

-- | Estimate price for @base/quote@ respecting freshness constraints.
oracleEstimate
  :: OracleAggregator
  -> FreshnessConstraints
  -> GYAssetClass
  -> GYAssetClass
  -> IO OracleQuote
oracleEstimate agg freshness base quote
  | base == quote = pure $ OracleQuote (PriceAverage (Price 1)) Nothing
  | requiresDerived = do
      baseAda <- oracleEstimateDirect agg freshness base GYLovelace
      quoteAda <- oracleEstimateDirect agg freshness quote GYLovelace
      pure $ combineDerivedQuotes baseAda quoteAda
  | otherwise = oracleEstimateDirect agg freshness base quote
 where
  requiresDerived = base /= GYLovelace && quote /= GYLovelace

-- | Estimate price with no freshness constraints.
oracleEstimateUnbounded :: OracleAggregator -> GYAssetClass -> GYAssetClass -> IO OracleQuote
oracleEstimateUnbounded agg = oracleEstimate agg noFreshnessConstraints

oracleEstimateDirect
  :: OracleAggregator
  -> FreshnessConstraints
  -> GYAssetClass
  -> GYAssetClass
  -> IO OracleQuote
oracleEstimateDirect OA {..} freshness base quote = do
  cached <- if ttl <= 0 then pure Nothing else fetchCached ttl key
  case cached of
    Just (_, quoteCached) -> do
      now <- oaClock
      if shouldReuse freshness base quote now quoteCached
        then pure quoteCached
        else refresh
    Nothing -> refresh
 where
  ttl = ocCacheDuration oaCfg
  key = (base, quote)

  refresh :: IO OracleQuote
  refresh = do
    quoteFresh <- freshEstimate
    when (ttl > 0) $ storeCached key quoteFresh
    pure quoteFresh

  fetchCached :: NominalDiffTime -> CacheKey -> IO (Maybe CacheEntry)
  fetchCached ttl' cacheKey = modifyMVar oaCache $ \cacheMap -> do
    now <- oaClock
    let (cacheMap', result) =
          case Map.lookup cacheKey cacheMap of
            Just entry@(ts, _)
              | diffUTCTime now ts < ttl' -> (cacheMap, Just entry)
              | otherwise -> (Map.delete cacheKey cacheMap, Nothing)
            Nothing -> (cacheMap, Nothing)
    pure (cacheMap', result)

  storeCached :: CacheKey -> OracleQuote -> IO ()
  storeCached cacheKey quoteFresh = do
    ts <- oaClock
    modifyMVar_ oaCache $ \cacheMap ->
      pure $ Map.insert cacheKey (ts, quoteFresh) cacheMap

  freshEstimate :: IO OracleQuote
  freshEstimate = do
    let
      provs = NE.toList oaProv
      weights = NE.toList oaWeights
    results <- mapM (\p -> ppGet p base quote) provs
    let
      zipped = zip3 provs weights results
      successes :: [(Int, OracleCertificate)]
      successes =
        [ (w, cert)
        | (_, w, Right cert) <- zipped,
          ocBaseAsset cert == base,
          ocQuoteAsset cert == quote
        ]
      mismatched =
        [ ppName provider
            ++ ":asset-mismatch"
        | (provider, _, Right cert) <- zipped,
          ocBaseAsset cert /= base || ocQuoteAsset cert /= quote
        ]
      errs =
        [ppName p | (p, _, Left _) <- zipped] ++ mismatched
    case successes of
      [] -> pure $ OracleQuote PriceUnavailable Nothing
      xs@((_, firstCert) : _) -> do
        let
          values = NE.fromList $ map (fromRational . getPrice . ocPrice . snd) xs
          rsd = relStdDev values
          t1 = ocThreshold1 oaCfg
          t2 = ocThreshold2 oaCfg
          price = ocPrice firstCert
          indicator
            | rsd > t2 = PriceMismatch2
            | rsd > t1 = PriceMismatch1
            | not (null errs) = PriceSourceFail errs price
            | otherwise = PriceAverage price
          certificate
            | indicatorHasPrice indicator = Just firstCert
            | otherwise = Nothing
        pure $ OracleQuote indicator certificate

shouldReuse :: FreshnessConstraints -> GYAssetClass -> GYAssetClass -> UTCTime -> OracleQuote -> Bool
shouldReuse FreshnessConstraints {..} expectedBase expectedQuote now OracleQuote {..}
  | fcValidFor <= 0 = True
  | otherwise = case oqCertificate of
      Nothing -> False
      Just OracleCertificate {ocTimestamp, ocBaseAsset, ocQuoteAsset} ->
        let
          age = diffUTCTime now ocTimestamp
          allowance = max 0 (fcValidFor - fcSigningSlack)
         in
          ocBaseAsset == expectedBase
            && ocQuoteAsset == expectedQuote
            && age <= allowance

indicatorHasPrice :: PriceIndicator -> Bool
indicatorHasPrice PriceAverage {} = True
indicatorHasPrice PriceSourceFail {} = True
indicatorHasPrice _ = False

combineDerivedQuotes :: OracleQuote -> OracleQuote -> OracleQuote
combineDerivedQuotes baseQuote quoteQuote =
  OracleQuote
    { oqIndicator = combineDerivedIndicators (oqIndicator baseQuote) (oqIndicator quoteQuote),
      oqCertificate = Nothing
    }

assetClassData :: GYAssetClass -> Data
assetClassData ac =
  case assetClassToPlutus ac of
    AssetClass (CurrencySymbol policyBs, TokenName assetBs) ->
      PlutusTx.Constr
        0
        [ PlutusTx.B (fromBuiltin policyBs),
          PlutusTx.B (fromBuiltin assetBs)
        ]

priceTimestampData :: GYAssetClass -> GYAssetClass -> Rational -> Integer -> Data
priceTimestampData baseAsset quoteAsset base ts =
  PlutusTx.Constr
    0
    [ assetClassData baseAsset,
      assetClassData quoteAsset,
      PlutusTx.Constr 0 [PlutusTx.I (numerator base), PlutusTx.I (denominator base)],
      PlutusTx.I ts
    ]

-- | Resolve cache TTL from the environment if provided.
resolveCacheDuration :: NominalDiffTime -> IO NominalDiffTime
resolveCacheDuration defaultTTL = do
  mEnv <- lookupEnv "ORACLE_CACHE_DURATION"
  case mEnv >>= (readMaybe :: String -> Maybe Double) of
    Just seconds | seconds >= 0 -> pure (realToFrac seconds)
    _ -> pure defaultTTL

-- | Create a mock provider with externally settable certificate.
newMockProvider
  :: String
  -> IO
       ( PriceProvider,
         Maybe OracleCertificate -> IO (),
         IO Int
       )
newMockProvider name = do
  mv <- newMVar Nothing
  hitsRef <- newIORef 0
  let
    setVal v = do
      _ <- swapMVar mv v
      pure ()
    getF _ _ = do
      modifyIORef' hitsRef (+ 1)
      v <- readMVar mv
      pure $ case v of
        Nothing -> Left "mock failure"
        Just cert -> Right cert
  pure
    ( PriceProvider name getF,
      setVal,
      readIORef hitsRef
    )

-- Statistics helpers ---------------------------------------------------------

relStdDev :: NonEmpty Double -> Double
relStdDev (x1 :| [x2]) = abs (x1 - x2) / (x1 + x2)
relStdDev xs = sqrt (mean ((\x -> (x - m) ^ (2 :: Int)) <$> xs)) / m
 where
  m = mean xs

mean :: Fractional a => NonEmpty a -> a
mean xs =
  let n = fromIntegral (length (NE.toList xs))
   in sum (NE.toList xs) / n

combineDerivedIndicators :: PriceIndicator -> PriceIndicator -> PriceIndicator
combineDerivedIndicators baseInd quoteInd =
  case (indicatorPrice baseInd, indicatorPrice quoteInd) of
    (Just (Price baseAda), Just (Price quoteAda))
      | quoteAda == 0 -> PriceUnavailable
      | otherwise ->
          let
            derivedPrice = Price (baseAda / quoteAda)
            severity = max (indicatorSeverity baseInd) (indicatorSeverity quoteInd)
            names = uniqueFailures baseInd quoteInd
           in
            case severity of
              SevUnavailable -> PriceUnavailable
              SevMismatch2 -> PriceMismatch2
              SevMismatch1 -> PriceMismatch1
              SevSourceFail -> PriceSourceFail names derivedPrice
              SevAverage -> PriceAverage derivedPrice
    _ -> worstIndicator baseInd quoteInd

worstIndicator :: PriceIndicator -> PriceIndicator -> PriceIndicator
worstIndicator i1 i2 =
  case max (indicatorSeverity i1) (indicatorSeverity i2) of
    SevUnavailable -> PriceUnavailable
    SevMismatch2 -> PriceMismatch2
    SevMismatch1 -> PriceMismatch1
    SevSourceFail ->
      let names = uniqueFailures i1 i2
       in case indicatorPrice i1 <|> indicatorPrice i2 of
            Just price -> PriceSourceFail names price
            Nothing -> PriceUnavailable
    SevAverage ->
      maybe
        PriceUnavailable
        PriceAverage
        (indicatorPrice i1 <|> indicatorPrice i2)

uniqueFailures :: PriceIndicator -> PriceIndicator -> [String]
uniqueFailures a b = nub (indicatorFailures a ++ indicatorFailures b)

indicatorPrice :: PriceIndicator -> Maybe Price
indicatorPrice (PriceAverage p) = Just p
indicatorPrice (PriceSourceFail _ p) = Just p
indicatorPrice _ = Nothing

indicatorFailures :: PriceIndicator -> [String]
indicatorFailures (PriceSourceFail names _) = names
indicatorFailures _ = []

data Severity = SevAverage | SevSourceFail | SevMismatch1 | SevMismatch2 | SevUnavailable
  deriving stock (Enum, Eq, Ord)

indicatorSeverity :: PriceIndicator -> Severity
indicatorSeverity PriceUnavailable = SevUnavailable
indicatorSeverity PriceMismatch2 = SevMismatch2
indicatorSeverity PriceMismatch1 = SevMismatch1
indicatorSeverity PriceAverage {} = SevAverage
indicatorSeverity PriceSourceFail {} = SevSourceFail
