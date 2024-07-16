module GeniusYield.Scripts.Dex.Version (
  POCVersion (..),
  defaultPOCVersion,
  SingPOCVersion (..),
  toSingPOCVersion,
  fromSingPOCVersion,
  SingPOCVersionI (..),
  SomeSingPOCVersion (..),
  withSomeSingPOCVersion,
) where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Default (Default (..))
import Data.GADT.Compare
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Type.Equality ((:~:) (..))
import GHC.Generics (Generic)
import GeniusYield.Imports ((&))

{- | Version of the family of partial order contracts.

>>> maxBound :: POCVersion
POCVersion1_1
-}
data POCVersion = POCVersion1 | POCVersion1_1
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default POCVersion where
  def = maxBound

-- | Same as @def@ but grep friendly.
defaultPOCVersion ∷ POCVersion
defaultPOCVersion = def

instance Swagger.ToParamSchema POCVersion where
  toParamSchema _ = mempty & Swagger.type_ ?~ Swagger.SwaggerString & Swagger.enum_ ?~ map Aeson.toJSON [minBound ∷ POCVersion .. maxBound]

instance Swagger.ToSchema POCVersion where
  declareNamedSchema p =
    pure $
      Swagger.named "POCVersion" $
        Swagger.paramSchemaToSchema p
          & Swagger.example
          ?~ Aeson.toJSON POCVersion1
            & Swagger.description
          ?~ "Version of the family of partial order contracts"

data SingPOCVersion (v ∷ POCVersion) where
  SingPOCVersion1 ∷ SingPOCVersion 'POCVersion1
  SingPOCVersion1_1 ∷ SingPOCVersion 'POCVersion1_1

data SomeSingPOCVersion where
  SomeSingPOCVersion ∷ SingPOCVersionI v ⇒ SingPOCVersion v → SomeSingPOCVersion

toSingPOCVersion ∷ POCVersion → SomeSingPOCVersion
toSingPOCVersion POCVersion1 = SomeSingPOCVersion SingPOCVersion1
toSingPOCVersion POCVersion1_1 = SomeSingPOCVersion SingPOCVersion1_1

fromSingPOCVersion ∷ SingPOCVersion v → POCVersion
fromSingPOCVersion SingPOCVersion1 = POCVersion1
fromSingPOCVersion SingPOCVersion1_1 = POCVersion1_1

withSomeSingPOCVersion ∷ SomeSingPOCVersion → (∀ v. SingPOCVersionI v ⇒ SingPOCVersion v → r) → r
withSomeSingPOCVersion (SomeSingPOCVersion s) f = f s

class SingPOCVersionI (v ∷ POCVersion) where
  singPOCVersion ∷ SingPOCVersion v

instance SingPOCVersionI 'POCVersion1 where
  singPOCVersion = SingPOCVersion1

instance SingPOCVersionI 'POCVersion1_1 where
  singPOCVersion = SingPOCVersion1_1

instance GEq SingPOCVersion where
  geq SingPOCVersion1 SingPOCVersion1 = Just Refl
  geq SingPOCVersion1 SingPOCVersion1_1 = Nothing
  geq SingPOCVersion1_1 SingPOCVersion1 = Nothing
  geq SingPOCVersion1_1 SingPOCVersion1_1 = Just Refl

instance GCompare SingPOCVersion where
  gcompare SingPOCVersion1 SingPOCVersion1 = GEQ
  gcompare SingPOCVersion1 SingPOCVersion1_1 = GLT
  gcompare SingPOCVersion1_1 SingPOCVersion1 = GGT
  gcompare SingPOCVersion1_1 SingPOCVersion1_1 = GEQ
