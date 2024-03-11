module GeniusYield.Server.Options where

-- TODO: Explicit export list.
-- TODO: Don't use printf as it's not type safe.

import Control.Exception (SomeException (..), displayException, try)
import Control.Monad.Trans.Except
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text.Lazy qualified as LT
import Data.Version (showVersion)
import Data.Yaml.Pretty qualified as Yaml
import Fmt
import GeniusYield.GYConfig
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.OrderBot.Adapter.Maestro (MaestroProvider (MaestroProvider))
import GeniusYield.Providers (networkIdToMaestroEnv)
import GeniusYield.Server.Api
import GeniusYield.Server.Auth
import GeniusYield.Server.Config (ServerConfig (..), coreConfigFromServerConfig, optionalSigningKeyFromServerConfig, serverConfigOptionalFPIO)
import GeniusYield.Server.Constants (gitHash)
import GeniusYield.Server.Ctx
import GeniusYield.Server.ErrorMiddleware
import GeniusYield.Server.RequestLoggerMiddleware (gcpReqLogger)
import GeniusYield.Server.Utils
import GeniusYield.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import PackageInfo_geniusyield_server_lib qualified as PackageInfo
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Internal.ServerError (responseServerError)
import System.TimeManager (TimeoutThread (..))

newtype Command = Serve ServeCommand

newtype ServeCommand = ServeCommand (Maybe FilePath)

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
    <$> optional
      ( strOption
          ( long "config"
              <> metavar "CONFIG"
              <> short 'c'
              <> help "Path of optional configuration file. If not provided, \"SERVER_CONFIG\" environment variable is used."
          )
      )

runCommand ∷ Command → IO ()
runCommand (Serve serveCommand) = runServeCommand serveCommand

runServeCommand ∷ ServeCommand → IO ()
runServeCommand (ServeCommand mcfp) = do
  serverConfig ← serverConfigOptionalFPIO mcfp
  menv ← networkIdToMaestroEnv (case scMaestroToken serverConfig of Confidential t → t) (scNetworkId serverConfig)
  let optionalSigningKey = optionalSigningKeyFromServerConfig serverConfig
      nid = scNetworkId serverConfig
      coreCfg = coreConfigFromServerConfig serverConfig
  withCfgProviders coreCfg "server" $ \providers → do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
    logInfoS $ "GeniusYield server version: " +| showVersion PackageInfo.version |+ "\nCommit used: " +| gitHash |+ ""
    -- TODO: Are the directories where these files are written fine?
    BL8.writeFile "web/swagger/api.json" (encodePretty geniusYieldAPISwagger)
    B.writeFile "web/swagger/api.yaml" (Yaml.encodePretty Yaml.defConfig geniusYieldAPISwagger)
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
          & Warp.setPort (scPort serverConfig)
          & Warp.setOnException onException
          & Warp.setOnExceptionResponse onExceptionResponse
      errLoggerMiddleware = errorLoggerMiddleware $ logErrorS . LT.unpack
      ctx =
        Ctx
          { ctxProviders = providers,
            ctxNetworkId = nid,
            ctxDexInfo =
              if
                | nid == GYMainnet → dexInfoDefaultMainnet
                | nid == GYTestnetPreprod → dexInfoDefaultPreprod
                | otherwise → error "Only mainnet & preprod network are supported",
            ctxMaestroProvider = MaestroProvider menv,
            ctxSigningKey = optionalSigningKey
          }

    logInfoS $
      "Starting GeniusYield server on port " +| scPort serverConfig |+ "\nCore config:\n" +| indentF 4 (fromString $ show coreCfg) |+ ""
    Warp.runSettings settings . reqLoggerMiddleware . errLoggerMiddleware . errorJsonWrapMiddleware $
      let context = apiKeyAuthHandler (case scServerApiKey serverConfig of Confidential t → apiKeyFromText t) :. EmptyContext
       in serveWithContext mainAPI context
            $ hoistServerWithContext
              mainAPI
              (Proxy ∷ Proxy '[AuthHandler Wai.Request ()])
              (\ioAct → Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try ioAct)
            $ mainServer ctx