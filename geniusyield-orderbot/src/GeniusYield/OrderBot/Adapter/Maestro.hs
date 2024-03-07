module GeniusYield.OrderBot.Adapter.Maestro where

import Control.Exception (try)
import GeniusYield.Imports (Exception, Text, throwIO)
import GeniusYield.OrderBot.Domain.Assets (adaAssetDetails)
import GeniusYield.OrderBot.Domain.Assets qualified as Domain
import GeniusYield.OrderBot.Domain.Markets qualified as Domain
import GeniusYield.OrderBot.Types (mkOrderAssetPair)
import GeniusYield.Providers.Common (silenceHeadersClientError)
import GeniusYield.Types (GYAssetClass (..), mintingPolicyIdToText, parseAssetClassWithSep, tokenNameToHex)
import Maestro.Client.V1
import Maestro.Types.V1
import RIO.Text (pack)

-- Exception utilities

data MaestroProviderException
  = MpeDeserializationException !Text
  | MpeRequestException !Text !MaestroError
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Remove headers (if `MaestroError` contains `ClientError`).
silenceHeadersMaestroClientError ∷ MaestroError → MaestroError
silenceHeadersMaestroClientError (ServantClientError e) = ServantClientError $ silenceHeadersClientError e
silenceHeadersMaestroClientError other = other

throwMpeApiError ∷ Text → MaestroError → IO a
throwMpeApiError locationInfo =
  throwIO . MpeRequestException locationInfo . silenceHeadersMaestroClientError

-- | Utility function to handle Maestro errors, which also removes header (if present) so as to conceal API key.
handleMaestroError ∷ Text → Either MaestroError a → IO a
handleMaestroError locationInfo = either (throwMpeApiError locationInfo) pure

newtype MaestroMarketsProvider = MaestroMarketsProvider (MaestroEnv 'V1)

instance Domain.HasAssets MaestroMarketsProvider where
  getAssetDetails (MaestroMarketsProvider menv) ac = case ac of
    GYLovelace → pure adaAssetDetails
    GYToken polId tkName → do
      AssetInfo {assetInfoTokenRegistryMetadata} ← try (getTimestampedData <$> assetInfo menv (NonAdaNativeToken (PolicyId . mintingPolicyIdToText $ polId) (TokenName . tokenNameToHex $ tkName))) >>= handleMaestroError (locationInfoPrefix <> "fetching particular token details")
      case assetInfoTokenRegistryMetadata of
        Nothing → pure $ Domain.AssetDetails ac Nothing Nothing
        Just TokenRegistryMetadata {tokenRegistryMetadataTicker, tokenRegistryMetadataDecimals} → pure $ Domain.AssetDetails ac (Domain.AssetTicker <$> tokenRegistryMetadataTicker) (Domain.AssetDecimals <$> tokenRegistryMetadataDecimals)
   where
    locationInfoPrefix = "getAssetDetails: "

instance Domain.HasMarkets MaestroMarketsProvider where
  getMarkets (MaestroMarketsProvider menv) = do
    DexPairResponse {dexPairResponsePairs} ← try (pairsFromDex menv GeniusYield) >>= handleMaestroError (locationInfoPrefix <> "fetching market pairs")
    traverse fromDexPairInfo dexPairResponsePairs
   where
    locationInfoPrefix = "getMarkets: "

    fromDexPairInfo DexPairInfo {..} = do
      a ← getAssetClass dexPairInfoCoinAPolicy dexPairInfoCoinAAssetName
      b ← getAssetClass dexPairInfoCoinBPolicy dexPairInfoCoinBAssetName
      pure $ mkOrderAssetPair a b

    getAssetClass (PolicyId polId) (TokenName tkName) =
      if polId == mempty -- TODO: Can this be polId <> tkName?
        then pure GYLovelace
        else case parseAssetClassWithSep '.' (polId <> "." <> tkName) of
          Left e → throwIO $ MpeDeserializationException (pack e)
          Right ac → pure ac