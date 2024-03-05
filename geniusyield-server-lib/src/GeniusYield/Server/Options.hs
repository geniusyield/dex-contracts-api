module GeniusYield.Server.Options where

-- TODO: Explicit export list.
-- TODO: Don't use printf as it's not type safe.

import Control.Exception (SomeException (..), displayException, try)
import Control.Monad.Trans.Except
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text.Lazy qualified as LT
import Data.Version (showVersion)
import Fmt
import GeniusYield.GYConfig
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.OrderBot.Adapter.Maestro.Markets (MaestroMarketsProvider (MaestroMarketsProvider))
import GeniusYield.Providers (networkIdToMaestroEnv)
import GeniusYield.Server.Api
import GeniusYield.Server.Constants (gitHash)
import GeniusYield.Server.Ctx
import GeniusYield.Server.ErrorMiddleware
import GeniusYield.Server.RequestLoggerMiddleware (gcpReqLogger)
import GeniusYield.Server.Utils
import GeniusYield.Types
import Maestro.Client.Env (mkMaestroEnv)
import Maestro.Types.V1 (Dex (..))
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import PackageInfo_geniusyield_server_lib qualified as PackageInfo
import Servant
import Servant.JS (vanillaJS, writeJSForAPI)
import Servant.Server.Internal.ServerError (responseServerError)
import System.TimeManager (TimeoutThread (..))

newtype Command = Serve ServeCommand

data ServeCommand = ServeCommand
  { serveCommandAtlasConfigPath ∷ FilePath,
    serveCommandMaestroKey ∷ Text,
    serveCommandMaestroDex ∷ Text
  }

parseCommand ∷ Parser Command
parseCommand =
  subparser $
    mconcat
      [ command
          "serve"
          ( info (Serve <$> parseServeCommand <**> helper) $
              progDesc "Serve endpoints"
          )
      ]

parseServeCommand ∷ Parser ServeCommand
parseServeCommand =
  ServeCommand
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> short 'c'
          <> help "Path of Atlas configuration file"
      )
    <*> strOption
      ( long "maestro-key"
          <> metavar "MAESTRO-KEY"
          <> short 'm'
          <> help "Maestro key to get markets information"
      )
    <*> strOption
      ( long "maestro-dex-provider"
          <> metavar "MAESTRO-DEX-PROVIDER"
          <> short 'd'
          <> help "DEX Provider for Maestro to get markets information from, available values are `minswap` and `genius-yield`"
      )

runCommand ∷ Command → IO ()
runCommand (Serve serveCommand) = runServeCommand serveCommand

runServeCommand ∷ ServeCommand → IO ()
runServeCommand (ServeCommand cfp mt md) = do
  coreCfg ← coreConfigIO cfp
  menv ← networkIdToMaestroEnv mt (cfgNetworkId coreCfg)
  let port = 8082
  withCfgProviders coreCfg "server" $ \providers → do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
    logInfoS $ "GeniusYield server version: " +| showVersion PackageInfo.version |+ "\nCommit used: " +| gitHash |+ ""
    -- TODO: Are the directories where these files are written fine?
    BL8.writeFile "web/swagger/api.json" (encodePretty geniusYieldAPISwagger)
    writeJSForAPI geniusYieldAPI vanillaJS "web/dist/api.js"
    reqLoggerMiddleware ← gcpReqLogger
    let
      -- These are only meant to catch fatal exceptions, application thrown exceptions should be caught beforehand.
      onException ∷ req → SomeException → IO ()
      onException _req exc =
        displayException exc
          & if isMatchedException exceptionsToIgnore exc
            then logInfoS
            else logErrorS
       where
        -- TimeoutThread and Warp.ConnectionClosedByPeer do not indicate that anything is wrong and
        -- should not be logged as errors. See
        -- https://magnus.therning.org/2021-07-03-the-timeout-manager-exception.html
        -- https://www.rfc-editor.org/rfc/rfc5246#page-29
        exceptionsToIgnore = Proxy @TimeoutThread :>> Proxy @Warp.InvalidRequest :>> ENil
      onExceptionResponse ∷ SomeException → Wai.Response
      onExceptionResponse _ = responseServerError . apiErrorToServerError $ someBackendError "Internal Server Error"
      settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setOnException onException
          & Warp.setOnExceptionResponse onExceptionResponse
      errLoggerMiddleware = errorLoggerMiddleware $ logErrorS . LT.unpack
      ctx =
        Ctx
          { ctxProviders = providers,
            ctxGYCoreConfig = coreCfg,
            ctxDexInfo =
              if
                | cfgNetworkId coreCfg == GYMainnet → dexInfoDefaultMainnet
                | cfgNetworkId coreCfg == GYTestnetPreprod → dexInfoDefaultPreprod
                | otherwise → error "Only mainnet & preprod network are supported",
            ctxMarketsProvider = MPMaestro (MaestroMarketsProvider menv (dexFromString md))
          }

    logInfoS $
      "Starting GeniusYield server on port " +| port |+ "\nCore config:\n" +| indentF 4 (fromString $ show coreCfg) |+ ""
    Warp.runSettings settings . reqLoggerMiddleware . errLoggerMiddleware . errorJsonWrapMiddleware $
      app ctx

app ∷ Ctx → Application
app ctx =
  serve mainAPI
    $ hoistServer
      mainAPI
      (\ioAct → Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try ioAct)
    $ mainServer ctx

-- FIXME: Get rid of this hack and use configs.
dexFromString "genius-yield" = GeniusYield
dexFromString "minswap" = Minswap
dexFromString _ = error "Invalid DEX provider"