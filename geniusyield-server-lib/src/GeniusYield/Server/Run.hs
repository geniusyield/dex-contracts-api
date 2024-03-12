module GeniusYield.Server.Run (
  runServer,
) where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Aeson.Encode.Pretty (encodePretty)
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
import PackageInfo_geniusyield_server_lib qualified as PackageInfo
import RIO hiding (Handler, logDebug, logErrorS, logInfo, logInfoS, onException)
import RIO.ByteString qualified as B
import RIO.ByteString.Lazy qualified as BL
import RIO.Text.Lazy qualified as LT
import Servant
-- import Servant.PY (requests, writePythonForAPI)
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Internal.ServerError (responseServerError)
import System.TimeManager (TimeoutThread (..))

runServer ∷ Maybe FilePath → IO ()
runServer mfp = do
  serverConfig ← serverConfigOptionalFPIO mfp
  menv ← networkIdToMaestroEnv (case scMaestroToken serverConfig of Confidential t → t) (scNetworkId serverConfig)
  let optionalSigningKey = optionalSigningKeyFromServerConfig serverConfig
      nid = scNetworkId serverConfig
      coreCfg = coreConfigFromServerConfig serverConfig
  -- writePythonForAPI (Proxy @MainAPI) requests "web/swagger/api.py"
  withCfgProviders coreCfg "server" $ \providers → do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
    logInfoS $ "GeniusYield server version: " +| showVersion PackageInfo.version |+ "\nCommit used: " +| gitHash |+ ""
    BL.writeFile "web/swagger/api.json" (encodePretty geniusYieldAPISwagger)
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