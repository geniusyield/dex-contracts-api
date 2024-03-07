module GeniusYield.OrderBot.Domain.Assets (
  AssetTicker (..),
  AssetDecimals (..),
  AssetDetails (..),
  adaAssetDetails,
  HasAssets (..),
) where

import Data.Aeson (ToJSON (..))
import Data.Swagger
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Swagger.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake)
import GeniusYield.Types (GYAssetClass (..))
import RIO (Word64, (&))
import RIO.Text (Text)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import           Data.Proxy
>>> import qualified Data.Swagger               as Swagger
-}

newtype AssetTicker = AssetTicker Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

instance Swagger.ToSchema AssetTicker where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Ticker of an asset."
      & addSwaggerExample "ADA"

newtype AssetDecimals = AssetDecimals Word64
  deriving stock (Show, Eq, Generic)
  deriving newtype (Num, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

instance Swagger.ToSchema AssetDecimals where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Decimals of an asset."
      & addSwaggerExample (toJSON (6 ∷ AssetDecimals))

type AssetDetailsPrefix ∷ Symbol
type AssetDetailsPrefix = "ad"

data AssetDetails = AssetDetails
  { adAsset ∷ !GYAssetClass,
    adAssetTicker ∷ !(Maybe AssetTicker),
    adAssetDecimals ∷ !(Maybe AssetDecimals)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix AssetDetailsPrefix, CamelToSnake]] AssetDetails

-- >>> Aeson.encode (Swagger.toSchema (Proxy :: Proxy AssetDetails))
-- "{\"description\":\"Asset details.\",\"required\":[\"asset\"],\"properties\":{\"asset\":{\"$ref\":\"#/definitions/GYAssetClass\"},\"asset_ticker\":{\"$ref\":\"#/definitions/AssetTicker\"},\"asset_decimals\":{\"$ref\":\"#/definitions/AssetDecimals\"}},\"type\":\"object\"}"
instance Swagger.ToSchema AssetDetails where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @AssetDetailsPrefix}
      & addSwaggerDescription "Asset details."

adaAssetDetails ∷ AssetDetails
adaAssetDetails =
  AssetDetails
    { adAssetTicker = Just $ AssetTicker "ADA",
      adAssetDecimals = Just 6,
      adAsset = GYLovelace
    }

class HasAssets a where
  getAssetDetails ∷ a → GYAssetClass → IO AssetDetails
