{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Server.Auth (
  V0,
  ApiKey,
  apiKeyFromText,
  ApiKeyHeader,
  apiKeyHeaderText,
  apiKeyAuthHandler,
  APIKeyAuthProtect,
) where

import Control.Lens (at, (?~))
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.Swagger
import GHC.TypeLits (Symbol, symbolVal)
import Network.Wai (Request (requestHeaders))
import RIO
import RIO.Text qualified as T
import Servant
import Servant.Foreign
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Servant.Swagger

type V0 ∷ Symbol
type V0 = "v0"

-- | The Api Key type.
newtype ApiKey = ApiKey ByteString

apiKeyFromText ∷ Text → ApiKey
apiKeyFromText = ApiKey . encodeUtf8

type ApiKeyHeader ∷ Symbol
type ApiKeyHeader = "api-key"

apiKeyHeaderText ∷ Text
apiKeyHeaderText = symbolVal (Proxy ∷ Proxy ApiKeyHeader) & T.pack

apiKeyAuthHandler ∷ ApiKey → AuthHandler Request ()
apiKeyAuthHandler (ApiKey key) = mkAuthHandler handler
 where
  handler req = case lookup "api-key" (requestHeaders req) of
    Nothing → throwError err401 {errBody = "Missing API key (please pass the api key in the api-key HTTP header)"}
    Just key' →
      if key' == key
        then pure ()
        else throwError err403 {errBody = "Invalid API key"}

type APIKeyAuthProtect = AuthProtect ApiKeyHeader

type instance AuthServerData APIKeyAuthProtect = ()

instance HasSwagger api ⇒ HasSwagger (APIKeyAuthProtect :> api) where
  toSwagger _ =
    toSwagger (Proxy ∷ Proxy api)
      & securityDefinitions
        .~ SecurityDefinitions (IOHM.fromList [(apiKeyHeaderText, apiKeySecurityScheme)])
      -- & paths
      --   . at signingKeyReqEndpoint
      --   . _Just
      --   . post
      --   . _Just
      --   . responses
      --   %~ add500SigningKeyFailureResponse
      -- & paths
      --   . at signingKeyReqEndpoint
      --   . _Just
      --   . delete
      --   . _Just
      --   . responses
      --   %~ add500SigningKeyFailureResponse
      & allOperations
        . security
        .~ [SecurityRequirement (IOHM.singleton apiKeyHeaderText [])]
      & allOperations
        . responses
        %~ addCommonResponses
   where
    apiKeySecurityScheme ∷ SecurityScheme
    apiKeySecurityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams apiKeyHeaderText ApiKeyHeader),
          _securitySchemeDescription = Just "API key for accessing the server's API."
        }
    addCommonResponses ∷ Responses → Responses
    addCommonResponses resps = resps & at 401 ?~ Inline response401 & at 403 ?~ Inline response403 & at 500 ?~ Inline response500

    -- add500SigningKeyFailureResponse ∷ Responses → Responses
    -- add500SigningKeyFailureResponse resps = resps & at 500 ?~ Inline response500SigningKeyFailure

    response401 ∷ Response
    response401 = mempty & description .~ "Unauthorized access - API key missing"

    response403 ∷ Response
    response403 = mempty & description .~ "Forbidden - The API key does not have permission to perform the request"

    response500 ∷ Response
    response500 = mempty & description .~ "Internal server error"

-- response500SigningKeyFailure ∷ Response
-- response500SigningKeyFailure = mempty & description .~ "Internal server error - Corresponding signing key is not configured"

-- signingKeyReqEndpoint = "/" <> symbolVal (Proxy ∷ Proxy V0) <> "/orders"

-- `HasForeign` instance for `APIKeyAuthProtect :> api` is required to generate client code using libraries such as `servant-py`.
-- This is written with help from https://github.com/haskell-servant/servant-auth/issues/8#issue-185541839.
instance ∀ lang ftype api. (HasForeign lang ftype api, HasForeignType lang ftype Text) ⇒ HasForeign lang ftype (APIKeyAuthProtect :> api) where
  type Foreign ftype (APIKeyAuthProtect :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy subR = foreignFor lang Proxy (Proxy ∷ Proxy api) subR'
   where
    subR' = subR {_reqHeaders = HeaderArg arg : _reqHeaders subR}
    arg =
      Arg
        { _argName = "api-key",
          _argType = typeFor lang (Proxy ∷ Proxy ftype) (Proxy ∷ Proxy Text)
        }
