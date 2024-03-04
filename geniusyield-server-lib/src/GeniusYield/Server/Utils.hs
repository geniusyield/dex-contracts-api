{-# LANGUAGE AllowAmbiguousTypes #-}

module GeniusYield.Server.Utils (
  ExceptionTypes (..),
  isMatchedException,
  logInfo,
  logDebug,
  pubKeyFromAddress,
  dropAndCamelToSnake,
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

dropAndCamelToSnake ∷ ∀ a. KnownSymbol a ⇒ String → String
dropAndCamelToSnake = camelTo2 '_' . drop (length $ symbolVal (Proxy @a))

addSwaggerDescription ∷ (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasDescription a (Maybe b2)) ⇒ b2 → f1 (f2 b1) → f1 (f2 b1)
addSwaggerDescription desc = mapped . mapped . Swagger.schema . Swagger.description ?~ desc

addSwaggerExample ∷ (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasExample a (Maybe b2)) ⇒ b2 → f1 (f2 b1) → f1 (f2 b1)
addSwaggerExample ex = mapped . mapped . Swagger.schema . Swagger.example ?~ ex