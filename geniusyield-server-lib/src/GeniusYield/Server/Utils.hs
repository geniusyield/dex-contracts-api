{-# LANGUAGE AllowAmbiguousTypes #-}

module GeniusYield.Server.Utils (
  ExceptionTypes (..),
  isMatchedException,
  logInfo,
  logDebug,
  dropSymbolAndCamelToSnake,
  addSwaggerDescription,
  addSwaggerExample,
) where

import Control.Exception
import GeniusYield.Imports
import GeniusYield.Server.Ctx
import GeniusYield.Swagger.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake)
import GeniusYield.Types

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