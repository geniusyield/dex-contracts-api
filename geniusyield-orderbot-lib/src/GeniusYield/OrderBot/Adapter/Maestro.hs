module GeniusYield.OrderBot.Adapter.Maestro (
  MaestroProvider (..),
  handleMaestroError,
) where

import GeniusYield.OrderBot.Domain.Assets (adaAssetDetails)
import GeniusYield.OrderBot.Domain.Assets qualified as Domain
import GeniusYield.OrderBot.Domain.Markets qualified as Domain
import GeniusYield.Types (GYAssetClass (..), mintingPolicyIdToText, parseAssetClassWithSep, tokenNameToHex)
import Maestro.Client.V1
import Maestro.Types.V1
import RIO
import RIO.Text (pack)

-- Exception utilities

data MaestroProviderException
  = MpeDeserializationException !Text
  | MpeRequestException !Text !MaestroError
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Utility function to handle Maestro errors.
handleMaestroError ∷ Text → Either MaestroError a → IO a
handleMaestroError locationInfo = either (throwIO . MpeRequestException locationInfo) pure

newtype MaestroProvider = MaestroProvider (MaestroEnv 'V1)

instance Domain.HasAssets MaestroProvider where
  getAssetDetails (MaestroProvider menv) ac = case ac of
    GYLovelace → pure adaAssetDetails
    GYToken polId tkName → do
      AssetInfo {assetInfoTokenRegistryMetadata} ← try (getTimestampedData <$> assetInfo menv (NonAdaNativeToken (PolicyId . mintingPolicyIdToText $ polId) (TokenName . tokenNameToHex $ tkName))) >>= handleMaestroError (locationInfoPrefix <> "fetching particular token details")
      case assetInfoTokenRegistryMetadata of
        Nothing → pure $ Domain.AssetDetails ac Nothing Nothing
        Just TokenRegistryMetadata {tokenRegistryMetadataTicker, tokenRegistryMetadataDecimals} → pure $ Domain.AssetDetails ac (Domain.AssetTicker <$> tokenRegistryMetadataTicker) (Domain.AssetDecimals <$> tokenRegistryMetadataDecimals)
   where
    locationInfoPrefix = "getAssetDetails: "

instance Domain.HasMarkets MaestroProvider where
  getMarkets (MaestroProvider menv) = do
    DexPairResponse {dexPairResponsePairs} ← try (pairsFromDex menv GeniusYield) >>= handleMaestroError (locationInfoPrefix <> "fetching market pairs")
    traverse fromDexPairInfo dexPairResponsePairs
   where
    locationInfoPrefix = "getMarkets: "

    fromDexPairInfo DexPairInfo {..} = do
      a ← getAssetClass dexPairInfoCoinAPolicy dexPairInfoCoinAAssetName
      b ← getAssetClass dexPairInfoCoinBPolicy dexPairInfoCoinBAssetName
      pure $ Domain.mkOrderAssetPair a b

    getAssetClass (PolicyId polId) (TokenName tkName) =
      if polId == mempty
        then pure GYLovelace
        else case parseAssetClassWithSep '.' (polId <> "." <> tkName) of
          Left e → throwIO $ MpeDeserializationException (pack e)
          Right ac → pure ac
