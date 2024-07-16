module GeniusYield.Server.Dex.HistoricalPrices.TapTools.Client (
  TapToolsUnit (..),
  TapToolsInterval (..),
  TapToolsOHLCV (..),
  TapToolsAPI,
  TapToolsOHLCVAPI,
  tapToolsClientEnv,
  tapToolsOHLCV,
  TapToolsException,
  handleTapToolsError,
) where

import Control.Lens ((?~))
import Data.Aeson (ToJSON (..))
import Data.Swagger qualified as Swagger
import Data.Time.Clock.POSIX
import Deriving.Aeson
import GHC.TypeLits (Symbol, symbolVal)
import GeniusYield.Server.Ctx (TapToolsApiKey, TapToolsEnv (tteApiKey, tteClientEnv))
import GeniusYield.Server.Utils (commonEnumParamSchemaRecipe, hideServantClientErrorHeader)
import GeniusYield.Swagger.Utils
import GeniusYield.Types (GYAssetClass)
import Maestro.Types.Common (LowerFirst)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO
import RIO.Text qualified as Text
import Servant.API
import Servant.Client

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import GeniusYield.Types
-}

newtype TapToolsUnit = TapToolsUnit {unTapToolsUnit ∷ GYAssetClass}
  deriving stock (Eq, Ord, Show)

{- |

>>> toUrlPiece $ TapToolsUnit "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53"
"dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb0014df1047454e53"
-}
instance ToHttpApiData TapToolsUnit where
  toUrlPiece (TapToolsUnit ac) = removeDot $ toUrlPiece ac
   where
    removeDot = Text.filter (/= '.')

data TapToolsInterval = TTI3m | TTI5m | TTI15m | TTI30m | TTI1h | TTI2h | TTI4h | TTI12h | TTI1d | TTI3d | TTI1w | TTI1M
  deriving stock (Eq, Ord, Enum, Bounded, Data, Typeable, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "TTI"]] TapToolsInterval

-- >>> show TTI1M
-- "1M"
instance Show TapToolsInterval where
  show = toConstr >>> show >>> drop 3

instance ToHttpApiData TapToolsInterval where
  toQueryParam = Text.pack . show

instance FromHttpApiData TapToolsInterval where
  parseQueryParam = \case
    "3m" → Right TTI3m
    "5m" → Right TTI5m
    "15m" → Right TTI15m
    "30m" → Right TTI30m
    "1h" → Right TTI1h
    "2h" → Right TTI2h
    "4h" → Right TTI4h
    "12h" → Right TTI12h
    "1d" → Right TTI1d
    "3d" → Right TTI3d
    "1w" → Right TTI1w
    "1M" → Right TTI1M
    x → Left $ "Invalid TapToolsInterval: " <> x

instance Swagger.ToParamSchema TapToolsInterval where
  toParamSchema = commonEnumParamSchemaRecipe

instance Swagger.ToSchema TapToolsInterval where
  declareNamedSchema p =
    pure
      $ Swagger.NamedSchema (Just "TapToolsInterval")
      $ Swagger.paramSchemaToSchema p
      & Swagger.example
      ?~ toJSON TTI1M
        & Swagger.description
      ?~ "The time interval"

type TapToolsOHLCVPrefix ∷ Symbol
type TapToolsOHLCVPrefix = "tapToolsOHLCV"

data TapToolsOHLCV = TapToolsOHLCV
  { tapToolsOHLCVTime ∷ !POSIXTime,
    tapToolsOHLCVOpen ∷ !Double,
    tapToolsOHLCVHigh ∷ !Double,
    tapToolsOHLCVLow ∷ !Double,
    tapToolsOHLCVClose ∷ !Double,
    tapToolsOHLCVVolume ∷ !Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TapToolsOHLCVPrefix, LowerFirst]] TapToolsOHLCV

instance Swagger.ToSchema TapToolsOHLCV where
  declareNamedSchema =
    let open = 0.15800583264941748
     in Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @TapToolsOHLCVPrefix}
          & addSwaggerDescription "Get a specific token's trended (open, high, low, close, volume) price data."
          & addSwaggerExample (toJSON $ TapToolsOHLCV {tapToolsOHLCVTime = 1_715_007_300, tapToolsOHLCVOpen = open, tapToolsOHLCVHigh = open, tapToolsOHLCVLow = open, tapToolsOHLCVClose = open, tapToolsOHLCVVolume = 120})

type TapToolsApiKeyHeaderName ∷ Symbol
type TapToolsApiKeyHeaderName = "x-api-key"

type TapToolsAPI =
  Header' '[Required] TapToolsApiKeyHeaderName TapToolsApiKey :> TapToolsOHLCVAPI

type TapToolsOHLCVAPI =
  "token"
    :> "ohlcv"
    :> QueryParam "unit" TapToolsUnit
    :> QueryParam' '[Required, Strict] "interval" TapToolsInterval
    :> QueryParam "numIntervals" Natural
    :> Get '[JSON] [TapToolsOHLCV]

_tapToolsOHLCV ∷ TapToolsApiKey → Maybe TapToolsUnit → TapToolsInterval → Maybe Natural → ClientM [TapToolsOHLCV]
_tapToolsOHLCV = client (Proxy @TapToolsAPI)

tapToolsBaseUrl ∷ String
tapToolsBaseUrl = "https://openapi.taptools.io/api/v1"

tapToolsClientEnv ∷ IO ClientEnv
tapToolsClientEnv = do
  baseUrl ← parseBaseUrl tapToolsBaseUrl
  manager ← newManager tlsManagerSettings
  pure $ mkClientEnv manager baseUrl

runTapToolsClient ∷ TapToolsEnv → ClientM a → IO (Either ClientError a)
runTapToolsClient (tteClientEnv → ce) c = runClientM c ce

-- | Exceptions.
data TapToolsException
  = -- | Error from the TapTools API.
    TapToolsApiError !Text !ClientError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

handleTapToolsError ∷ Text → Either ClientError a → IO a
handleTapToolsError locationInfo = either (throwIO . TapToolsApiError locationInfo . hideServantClientErrorHeader (fromString $ symbolVal (Proxy @TapToolsApiKeyHeaderName))) pure

tapToolsOHLCV ∷ TapToolsEnv → Maybe TapToolsUnit → TapToolsInterval → Maybe Natural → IO [TapToolsOHLCV]
tapToolsOHLCV env@(tteApiKey → apiKey) ttu tti mttni = _tapToolsOHLCV apiKey ttu tti mttni & runTapToolsClient env >>= handleTapToolsError "tapToolsOHLCV"
