module GeniusYield.Server.Api (
  GeniusYieldAPI,
  geniusYieldAPI,
  geniusYieldServer,
  MainAPI,
  mainAPI,
  mainServer,
  geniusYieldAPISwagger,
) where

import Control.Lens ((.~), (?~))
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Swagger
import Data.Swagger qualified as Swagger
import Data.Version (showVersion)
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.Imports
import GeniusYield.Server.Constants (gitHash)
import GeniusYield.Server.Ctx
import GeniusYield.Server.Dex.PartialOrder (DEXPartialOrderAPI, handleDEXPartialOrder)
import GeniusYield.Server.Dex.Markets (MarketsAPI, handleMarketsApi)
import GeniusYield.Server.Utils
import GeniusYield.Types
import PackageInfo_geniusyield_server_lib qualified as PackageInfo
import Servant
import Servant.Swagger

type SettingsPrefix ∷ Symbol
type SettingsPrefix = "settings"

data Settings = Settings
  { settingsNetwork ∷ !String,
    settingsVersion ∷ !String,
    settingsRevision ∷ !String,
    settingsBackend ∷ !String
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SettingsPrefix, CamelToSnake]] Settings

instance Swagger.ToSchema Settings where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @SettingsPrefix}
      & addSwaggerDescription "Genius Yield Server settings."

-------------------------------------------------------------------------------
-- Server's API
-------------------------------------------------------------------------------

type GeniusYieldAPI =
  "settings" :> Get '[JSON] Settings
    :<|> DEXPartialOrderAPI
    :<|> MarketsAPI

geniusYieldAPI ∷ Proxy GeniusYieldAPI
geniusYieldAPI = Proxy

geniusYieldAPISwagger ∷ Swagger
geniusYieldAPISwagger =
  toSwagger geniusYieldAPI
    & info
    . title
    .~ "Genius Yield Haskell Server API"
      & info
      . version
    .~ "1.0"
      & info
      . description
    ?~ "This API is used for building and submitting transactions."

geniusYieldServer ∷ Ctx → ServerT GeniusYieldAPI IO
geniusYieldServer ctx =
  handleSettings ctx
    :<|> handleDEXPartialOrder ctx
    :<|> handleMarketsApi ctx

type MainAPI =
  GeniusYieldAPI
    :<|> "ui" :> Raw
    :<|> "swagger" :> Raw

mainAPI ∷ Proxy MainAPI
mainAPI = Proxy

mainServer ∷ Ctx → ServerT MainAPI IO
mainServer ctx =
  geniusYieldServer ctx
    :<|> serveDirectoryFileServer "web/dist"
    :<|> serveDirectoryFileServer "web/swagger"

handleSettings ∷ Ctx → IO Settings
handleSettings ctx@Ctx {..} = do
  logInfo ctx "Settings requested."
  pure $ Settings {settingsNetwork = ctxGYCoreConfig & cfgNetworkId & customShowNetworkId, settingsVersion = showVersion PackageInfo.version, settingsRevision = gitHash, settingsBackend = "mmb"}

-- >>> customShowNetworkId GYMainnet
-- "mainnet"
-- >>> customShowNetworkId GYTestnetLegacy
-- "legacy"
-- >>> customShowNetworkId GYPrivnet
-- "privnet"
customShowNetworkId ∷ GYNetworkId → String
customShowNetworkId = show >>> removePrefix "GY" >>> removePrefix "Testnet" >>> lowerFirstChar
 where
  removePrefix ∷ String → String → String
  removePrefix pref str
    | pref `isPrefixOf` str = drop (length pref) str
    | otherwise = str
  lowerFirstChar ∷ String → String
  lowerFirstChar "" = ""
  lowerFirstChar (x : xs) = toLower x : xs
