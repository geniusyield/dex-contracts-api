{-# LANGUAGE AllowAmbiguousTypes #-}

module GeniusYield.Server.Utils (
  ExceptionTypes (..),
  isMatchedException,
  logInfo,
  logDebug,
  pubKeyFromAddress,
  dropSymbolAndCamelToSnake,
  addSwaggerDescription,
  addSwaggerExample,
) where

import Control.Exception
import Control.Lens (mapped, (?~))
import Data.Aeson (camelTo2)
import Data.Swagger qualified as Swagger
import GHC.TypeLits
import GeniusYield.Imports
import GeniusYield.Server.Ctx
import GeniusYield.Swagger.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake)
import GeniusYield.Types

newtype NoPubKeyAddressError = NoPubKeyAddressError GYAddressBech32
  deriving stock (Show)
  deriving anyclass (Exception)

logDebug ∷ HasCallStack ⇒ Ctx → String → IO ()
logDebug ctx = gyLogDebug (ctxProviders ctx) mempty

logInfo ∷ HasCallStack ⇒ Ctx → String → IO ()
logInfo ctx = gyLogInfo (ctxProviders ctx) mempty

pubKeyFromAddress ∷ GYAddress → IO GYPubKeyHash
pubKeyFromAddress addr =
  maybe
    (throwIO $ NoPubKeyAddressError $ addressToBech32 addr)
    return
    (addressToPubKeyHash addr)

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