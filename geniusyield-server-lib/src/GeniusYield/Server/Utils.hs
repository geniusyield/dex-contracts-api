{-# LANGUAGE AllowAmbiguousTypes #-}

module GeniusYield.Server.Utils (
  ExceptionTypes (..),
  isMatchedException,
  logInfo,
  logDebug,
  dropSymbolAndCamelToSnake,
  addSwaggerDescription,
  addSwaggerExample,
  bytestringToString,
  hideServantClientErrorHeader,
  commonEnumParamSchemaRecipe,
) where

import Control.Lens ((?~))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal qualified as Swagger
import GeniusYield.Imports
import GeniusYield.Server.Ctx
import GeniusYield.Swagger.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake)
import GeniusYield.Types
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types qualified as Http
import RIO hiding (logDebug, logInfo)
import RIO.Text qualified as Text
import Servant.Client qualified as Servant
import Servant.Client.Core qualified as Servant

logDebug ∷ HasCallStack ⇒ Ctx → String → IO ()
logDebug ctx = gyLogDebug (ctxProviders ctx) mempty

logInfo ∷ HasCallStack ⇒ Ctx → String → IO ()
logInfo ctx = gyLogInfo (ctxProviders ctx) mempty

type ExceptionTypes ∷ [Type] → Type
data ExceptionTypes es where
  ENil ∷ ExceptionTypes '[]
  (:>>) ∷ Exception e ⇒ Proxy e → ExceptionTypes es → ExceptionTypes (e ': es)

infixr 5 :>>

isMatchedException ∷ ExceptionTypes es → SomeException → Bool
isMatchedException ENil _ = False
isMatchedException (etype :>> etypes) se = isJust (f etype) || isMatchedException etypes se
 where
  f ∷ ∀ e. Exception e ⇒ Proxy e → Maybe e
  f _ = fromException @e se

bytestringToString ∷ ByteString → String
bytestringToString = RIO.decodeUtf8Lenient >>> Text.unpack

hideServantClientErrorHeader ∷ Http.HeaderName → Servant.ClientError → Servant.ClientError
hideServantClientErrorHeader headerName clientError = case clientError of
  Servant.FailureResponse reqF res → Servant.FailureResponse reqF {Servant.requestHeaders = renameHeader <$> Servant.requestHeaders reqF} res
  Servant.ConnectionError se → case fromException @Http.HttpException se of
    Just he → case he of
      Http.HttpExceptionRequest req content → Servant.ConnectionError $ SomeException $ Http.HttpExceptionRequest req {Http.requestHeaders = renameHeader <$> Http.requestHeaders req} content
      _anyOther → clientError
    Nothing → clientError
  _anyOther → clientError
 where
  renameHeader (h, v) = if h == headerName then (h, "hidden") else (h, v)

commonEnumParamSchemaRecipe ∷ ∀ a (t ∷ Swagger.SwaggerKind Type). (Bounded a, Enum a, ToJSON a) ⇒ Proxy a → Swagger.ParamSchema t
commonEnumParamSchemaRecipe _ = mempty & Swagger.type_ ?~ Swagger.SwaggerString & Swagger.enum_ ?~ fmap toJSON [(minBound ∷ a) .. maxBound]
