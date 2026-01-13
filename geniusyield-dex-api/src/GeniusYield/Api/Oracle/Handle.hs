module GeniusYield.Api.Oracle.Handle (
  OracleHandle,
  oracleHandleFromAggregator,
  oracleHandleEstimate,
  oracleHandleEstimateUnbounded,
  oracleHandleDiagnostics,
  buildOracleHandle,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import GeniusYield.Api.Oracle
import GeniusYield.Api.Oracle.Providers
import GeniusYield.Config.DEX (GYOracleConfig)
import GeniusYield.Types (GYAssetClass, GYNetworkId, GYPaymentSigningKey)

-- | Runtime oracle handle that owns the aggregator cache.
data OracleHandle = OracleHandle
  { ohAggregator :: !OracleAggregator,
    ohDiagnostics :: ![Text]
  }

-- | Build an 'OracleHandle' around an existing aggregator and diagnostics bundle.
oracleHandleFromAggregator :: OracleAggregator -> [Text] -> OracleHandle
oracleHandleFromAggregator aggregator diagnostics =
  OracleHandle
    { ohAggregator = aggregator,
      ohDiagnostics = diagnostics
    }

oracleHandleDiagnostics :: OracleHandle -> [Text]
oracleHandleDiagnostics = ohDiagnostics

oracleHandleEstimate
  :: OracleHandle
  -> FreshnessConstraints
  -> GYAssetClass
  -> GYAssetClass
  -> IO OracleQuote
oracleHandleEstimate OracleHandle {ohAggregator} = oracleEstimate ohAggregator

oracleHandleEstimateUnbounded
  :: OracleHandle
  -> GYAssetClass
  -> GYAssetClass
  -> IO OracleQuote
oracleHandleEstimateUnbounded OracleHandle {ohAggregator} = oracleEstimateUnbounded ohAggregator

buildOracleHandle
  :: GYNetworkId
  -> GYPaymentSigningKey
  -> GYOracleConfig
  -> IO (Either OracleProviderInitError OracleHandle)
buildOracleHandle nid signingKey cfg = do
  bundleResult <- buildOracleProviderBundle nid signingKey cfg
  case bundleResult of
    Left err -> pure (Left err)
    Right bundle -> do
      aggregator <- mkAggregator (opbCfg bundle) (NE.toList (opbProviders bundle)) (NE.toList (opbWeights bundle))
      pure $
        Right
          OracleHandle
            { ohAggregator = aggregator,
              ohDiagnostics = opbDiagnostics bundle
            }
