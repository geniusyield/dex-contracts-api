module GeniusYield.Server.Config (
  ServerConfig (..),
  serverConfigOptionalFPIO,
  coreConfigFromServerConfig,
  optionalSigningKeyFromServerConfig,
) where

import Data.Aeson (
  eitherDecodeFileStrict,
  eitherDecodeStrict,
 )
import Data.Yaml qualified as Yaml
import Deriving.Aeson
import GeniusYield.GYConfig (Confidential, GYCoreConfig (..), GYCoreProviderInfo)
import GeniusYield.Types
import Maestro.Types.Common (LowerFirst)
import Network.Wai.Handler.Warp (Port)
import RIO (Bifunctor (first), IsString (..), Text, Word32, fromMaybe, throwIO)
import RIO.FilePath (takeExtension)
import System.Envy

data MnemonicWallet = MnemonicWallet
  { -- | Mnemonic (seed phrase).
    mnemonic ∷ !Mnemonic,
    -- | Account index.
    accIx ∷ !(Maybe Word32),
    -- | Payment address index.
    addrIx ∷ !(Maybe Word32)
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] MnemonicWallet

data ServerConfig = ServerConfig
  { scCoreProvider ∷ !GYCoreProviderInfo,
    scNetworkId ∷ !GYNetworkId,
    scLogging ∷ ![GYLogScribeConfig],
    scMaestroToken ∷ !(Confidential Text),
    scPort ∷ !Port,
    scMnemmonic ∷ !(Maybe MnemonicWallet),
    scServerApiKey ∷ !(Confidential Text)
  }
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "sc", LowerFirst]] ServerConfig

instance FromEnv ServerConfig where
  fromEnv _ = forceFromJson <$> env "SERVER_CONFIG" -- ServerConfig <$>  (forceFromJson <$> env "CORE_PROVIDER") <*> (forceFromJson <$> env "NETWORK_ID") <*> (forceFromJson <$> env "LOGGING") <*> (forceFromJson <$> env "MAESTRO_TOKEN") <*> (forceFromJson <$> env "MNEMONIC")
   where
    forceFromJson ∷ FromJSON a ⇒ String → a
    forceFromJson = either error id . eitherDecodeStrict . fromString

eitherDecodeFileStrictJsonOrYaml ∷ FromJSON a ⇒ FilePath → IO (Either String a)
eitherDecodeFileStrictJsonOrYaml fp =
  case takeExtension fp of
    ".json" → eitherDecodeFileStrict fp
    ".yaml" → first show <$> Yaml.decodeFileEither fp
    _ → throwIO $ userError "Only .json or .yaml extensions are supported for configuration."

serverConfigOptionalFPIO ∷ Maybe FilePath → IO ServerConfig
serverConfigOptionalFPIO mfp = do
  e ← maybe decodeEnv eitherDecodeFileStrictJsonOrYaml mfp
  either (throwIO . userError) return e

coreConfigFromServerConfig ∷ ServerConfig → GYCoreConfig
coreConfigFromServerConfig ServerConfig {..} =
  GYCoreConfig
    { cfgCoreProvider = scCoreProvider,
      cfgNetworkId = scNetworkId,
      cfgLogging = scLogging
    }

optionalSigningKeyFromServerConfig ∷ ServerConfig → Maybe GYSomePaymentSigningKey
optionalSigningKeyFromServerConfig ServerConfig {..} = do
  case scMnemmonic of
    Nothing → Nothing
    Just MnemonicWallet {..} →
      let wk' = walletKeysFromMnemonicIndexed mnemonic (fromMaybe 0 accIx) (fromMaybe 0 addrIx)
       in case wk' of
            Left _ → Nothing
            Right wk → Just $ AGYExtendedPaymentSigningKey $ walletKeysToExtendedPaymentSigningKey wk