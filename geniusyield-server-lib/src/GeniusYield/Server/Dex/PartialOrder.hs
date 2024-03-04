{-# LANGUAGE OverloadedLists #-}

module GeniusYield.Server.Dex.PartialOrder (
  DEXPartialOrderAPI,
  handleDEXPartialOrder,
) where

import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..))
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.HTTP.Errors (
  GYApiError (..),
  IsGYApiError (..),
 )
import GeniusYield.Imports
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils (addSwaggerDescription, dropAndCamelToSnake, logInfo)
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import Network.HTTP.Types (status400)
import Servant

type TradingFeesPrefix ∷ Symbol
type TradingFeesPrefix = "tf"

-- TODO: JSON & Swagger instances.
data TradingFees = TradingFees
  { tfFlatMakerFee ∷ !GYNatural,
    tfFlatTakerFee ∷ !GYNatural,
    tfPercentageMakerFee ∷ !GYRational,
    tfPercentageTakerFee ∷ !GYRational
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TradingFeesPrefix, CamelToSnake]] TradingFees

instance Swagger.ToSchema TradingFees where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @TradingFeesPrefix}
      & addSwaggerDescription "Trading fees of DEX."

type DEXPartialOrderAPI =
  "trading_fees"
    :> Get '[JSON] TradingFees

handleDEXPartialOrder ∷ Ctx → ServerT DEXPartialOrderAPI IO
handleDEXPartialOrder ctx =
  handleTradingFees ctx

handleTradingFees ∷ Ctx → IO TradingFees
handleTradingFees ctx@Ctx {..} = do
  logInfo ctx "Calculating trading fees."
  (_, pocd) ← runQuery ctx $ fetchPartialOrderConfig $ porRefNft $ dexPORefs $ ctxDexInfo
  pure
    TradingFees
      { tfFlatMakerFee = fromIntegral $ pociMakerFeeFlat pocd,
        tfFlatTakerFee = fromIntegral $ pociTakerFee pocd,
        tfPercentageMakerFee = 100 * pociMakerFeeRatio pocd,
        tfPercentageTakerFee = 100 * pociMakerFeeRatio pocd
      }
