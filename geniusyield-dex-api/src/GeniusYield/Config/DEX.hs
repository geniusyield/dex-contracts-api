module GeniusYield.Config.DEX (
  GYDEXConfig (..),
  GYOracleConfig (..),
  GYOracleProviderConfig (..),
  GYOracleAssetMappingConfig (..),
  MaestroOracleProviderConfig (..),
  MaestroPairOverrideConfig (..),
  TaptoolsOracleProviderConfig (..),
  TaptoolsPairOverrideConfig (..),
  Charli3OracleProviderConfig (..),
  Charli3PairOverrideConfig (..),
  dexConfigIO,
) where

import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.IO qualified as TextIO (readFile)
import Deriving.Aeson
import GeniusYield.Api.DEX.PartialOrder (PORefs)
import GeniusYield.Api.DEX.TwoWayOrder (TWORef)
import GeniusYield.Config.Utils (fillPlaceholders)
import GeniusYield.Types

data GYDEXConfig = GYDEXConfig
  { dexOracleSKeyPath :: !FilePath,
    dexFee :: !Rational,
    dexPORefs :: !PORefs,
    dexTWORef :: !TWORef,
    dexConfigAddrV1 :: !GYAddressBech32,
    dexConfigAddrV1_1 :: !GYAddressBech32,
    dexOracle :: !(Maybe GYOracleConfig)
  }
  deriving (FromJSON, Generic, Show, ToJSON)

data GYOracleConfig = GYOracleConfig
  { gocThreshold1 :: !(Maybe Double),
    gocThreshold2 :: !(Maybe Double),
    gocCacheSeconds :: !(Maybe Int),
    gocProviders :: !(NonEmpty GYOracleProviderConfig),
    gocAssetMappings :: !(Maybe [GYOracleAssetMappingConfig]),
    gocWeights :: !(Maybe (NonEmpty Int))
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "goc", CamelToSnake]] GYOracleConfig

data GYOracleAssetMappingConfig = GYOracleAssetMappingConfig
  { goamcPreprodAsset :: !Text,
    goamcMainnetAsset :: !Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "goamc", CamelToSnake]] GYOracleAssetMappingConfig

data GYOracleProviderConfig
  = GYOracleProviderMaestro MaestroOracleProviderConfig
  | GYOracleProviderTaptools TaptoolsOracleProviderConfig
  | GYOracleProviderCharli3 Charli3OracleProviderConfig
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "GYOracleProvider", CamelToSnake]] GYOracleProviderConfig

data MaestroOracleProviderConfig = MaestroOracleProviderConfig
  { mopcApiKey :: !Text,
    mopcDex :: !Text,
    mopcResolution :: !(Maybe Text),
    mopcPairOverrides :: !(Maybe [MaestroPairOverrideConfig])
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "mopc", CamelToSnake]] MaestroOracleProviderConfig

data MaestroPairOverrideConfig = MaestroPairOverrideConfig
  { mpocPair :: !Text,
    mpocCommodityIsFirst :: !Bool
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "mpoc", CamelToSnake]] MaestroPairOverrideConfig

data TaptoolsOracleProviderConfig = TaptoolsOracleProviderConfig
  { topcApiKey :: !Text,
    topcPairOverrides :: !(Maybe [TaptoolsPairOverrideConfig])
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "topc", CamelToSnake]] TaptoolsOracleProviderConfig

data TaptoolsPairOverrideConfig = TaptoolsPairOverrideConfig
  { tppcAsset :: !Text,
    tppcPrecision :: !Natural
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "tppc", CamelToSnake]] TaptoolsPairOverrideConfig

data Charli3OracleProviderConfig = Charli3OracleProviderConfig
  { c3opcApiKey :: !Text,
    c3opcPairOverrides :: !(Maybe [Charli3PairOverrideConfig])
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "c3opc", CamelToSnake]] Charli3OracleProviderConfig

data Charli3PairOverrideConfig = Charli3PairOverrideConfig
  { c3pocAsset :: !Text,
    c3pocPolicy :: !(Maybe Text),
    c3pocPool :: !(Maybe Text)
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "c3poc", CamelToSnake]] Charli3PairOverrideConfig

dexConfigIO :: FilePath -> IO GYDEXConfig
dexConfigIO file = do
  t <- TextIO.readFile file >>= fillPlaceholders
  case Aeson.eitherDecodeStrictText t of
    Left err -> throwIO $ userError err
    Right cfg -> pure cfg
