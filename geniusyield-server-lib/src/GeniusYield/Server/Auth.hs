module GeniusYield.Server.Auth (
  V0,
  ApiKey,
  apiKeyFromText,
  ApiKeyHeader,
  apiKeyHeaderText,
  apiKeyAuthHandler,
  APIKeyAuthProtect,
) where

import GHC.TypeLits (Symbol, symbolVal)
import Network.Wai (Request (requestHeaders))
import RIO
import RIO.Text qualified as T
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

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
