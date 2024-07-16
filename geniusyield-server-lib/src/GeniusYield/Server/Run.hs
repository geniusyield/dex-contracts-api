module GeniusYield.Server.Run (
  runServer,
) where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
-- import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Strict qualified as Strict
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
-- import RIO.ByteString.Lazy qualified as BL

-- import Servant.PY (requests, writePythonForAPI)

import GeniusYield.Server.Dex.HistoricalPrices.TapTools.Client (tapToolsClientEnv)
import GeniusYield.Server.ErrorMiddleware
import GeniusYield.Server.RequestLoggerMiddleware (gcpReqLogger)
import GeniusYield.Server.Utils
import GeniusYield.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import PackageInfo_geniusyield_server_lib qualified as PackageInfo
import RIO hiding (Handler, logDebug, logErrorS, logInfo, logInfoS, onException)
import RIO.ByteString qualified as B
import RIO.Text.Lazy qualified as LT
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Internal.ServerError (responseServerError)
import System.TimeManager (TimeoutThread (..))

runServer ∷ Maybe FilePath → IO ()
runServer mfp = do
  serverConfig ← serverConfigOptionalFPIO mfp
  menv ← networkIdToMaestroEnv (case scMaestroToken serverConfig of Confidential t → t) (scNetworkId serverConfig)
  mtenv ←
    case scTapToolsApiKey serverConfig of
      Nothing → pure Nothing
      Just (Confidential apiKey) → do
        tce ← tapToolsClientEnv
        pure $ Just $ TapToolsEnv {tteClientEnv = tce, tteApiKey = apiKey}
  optionalSigningKey ← optionalSigningKeyFromServerConfig serverConfig
  let nid = scNetworkId serverConfig
      coreCfg = coreConfigFromServerConfig serverConfig
  -- writePythonForAPI (Proxy @MainAPI) requests "web/swagger/api.py"
  withCfgProviders coreCfg "server" $ \providers → do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
    logInfoS $ "GeniusYield server version: " +| showVersion PackageInfo.version |+ "\nCommit used: " +| gitHash |+ "\nOptional collateral configuration: " +|| scCollateral serverConfig ||+ "\nAddress of optional wallet: " +|| fmap Strict.snd optionalSigningKey ||+ "\nOptional stake address: " +|| scStakeAddress serverConfig ||+ ""
    -- BL.writeFile "web/swagger/api.json" (encodePretty geniusYieldAPISwagger)
    B.writeFile "web/openapi/api.yaml" (Yaml.encodePretty Yaml.defConfig geniusYieldAPIOpenApi)
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
            ctxTapToolsProvider = mtenv,
            ctxSigningKey = optionalSigningKey,
            ctxCollateral = scCollateral serverConfig,
            ctxStakeAddress = scStakeAddress serverConfig
          }

    logInfoS
      $ "Starting GeniusYield server on port "
      +| scPort serverConfig
      |+ "\nCore config:\n"
      +| indentF 4 (fromString $ show coreCfg)
      |+ ""
    Warp.runSettings settings
      . reqLoggerMiddleware
      . errLoggerMiddleware
      . errorJsonWrapMiddleware
      $ let context = apiKeyAuthHandler (case scServerApiKey serverConfig of Confidential t → apiKeyFromText t) :. EmptyContext
         in serveWithContext mainAPI context
              $ hoistServerWithContext
                mainAPI
                (Proxy ∷ Proxy '[AuthHandler Wai.Request ()])
                (\ioAct → Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try ioAct)
              $ mainServer ctx
