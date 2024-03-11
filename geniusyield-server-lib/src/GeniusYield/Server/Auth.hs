module GeniusYield.Server.Auth (
  ApiKey,
  apiKeyFromText,
  ApiKeyHeader,
  apiKeyHeaderText,
  apiKeyAuthHandler,
) where

import GHC.TypeLits (Symbol, symbolVal)
import Network.Wai (Request (requestHeaders))
import RIO
import RIO.Text qualified as T
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

-- | The Api Key type.
newtype ApiKey = ApiKey ByteString

-- TODO: Can it be inferred from bytestring?
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
    Nothing → throwError err401 {errBody = "Missing API key"}
    Just key' →
      if key' == key
        then pure ()
        else throwError err403 {errBody = "Invalid API key"}