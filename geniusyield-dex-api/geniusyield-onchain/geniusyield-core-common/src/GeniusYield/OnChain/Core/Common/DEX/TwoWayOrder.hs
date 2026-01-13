{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module GeniusYield.OnChain.Core.Common.DEX.TwoWayOrder (
  TwoWayOrderDatum (..),
  TwoWayOrderPrice (..),
  TwoWayOrderPriceDelta (..),
  TwoWayOrder (..),
  TwoWayOrderWays (..),
  TwoWayOrderAssetDetails (..),
  TwoWayOrderOffer (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (type Generic)
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx.Ratio qualified as Tx

-- TODO: Add to Atlas

data TwoWayOrderDatum = TwoWayOrderDatum
  { twoiOwnerCredentials :: ![Credential],
    twoiOwnerAddr :: !Address,
    twoiNFT :: !TokenName,
    twoiOffer :: !(TwoWayOrderPrice TwoWayOrder),
    twoiStart :: !(Maybe POSIXTime),
    twoiEnd :: !(Maybe POSIXTime),
    twoiTakerLovelaceFlatFee :: !Integer,
    twoiTakerFeeRatio :: !Tx.Rational,
    twoiMakerFeeRatio :: !Tx.Rational,
    twoiOracleFreshnessSeconds :: !Integer
  }
  deriving stock (Generic, Show)

-- deriving anyclass (Swagger.ToSchema)

-- deriving anyclass (Swagger.ToSchema)

-- | Price along with timestamp.

-- deriving anyclass (Swagger.ToSchema)

-- | Price that is set in the order.
data TwoWayOrderPrice of'
  = TwoWayOrderPriceFixed !(of' Tx.Rational)
  | -- | Price given as a delta to oracle price.
    TwoWayOrderDynamic
      { twoPriceDelta :: !(of' TwoWayOrderPriceDelta),
        twoOracleKey :: !ByteString,
        twoToFlip :: !Bool
      }

deriving stock instance
  (Show (of' TwoWayOrderPriceDelta), Show (of' Tx.Rational))
  => Show (TwoWayOrderPrice of')

deriving stock instance
  (Eq (of' TwoWayOrderPriceDelta), Eq (of' Tx.Rational))
  => Eq (TwoWayOrderPrice of')

deriving stock instance
  (Generic (of' TwoWayOrderPriceDelta), Generic (of' Tx.Rational))
  => Generic (TwoWayOrderPrice of')

data TwoWayOrderPriceDelta = TwoWayOrderPriceDelta
  { twoOffset :: !Tx.Rational,
    twoSpread :: !Tx.Rational
  }
  deriving stock (Eq, Generic, Show)

-- deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

newtype TwoWayOrder price = TwoWayOrder (TwoWayOrderWays (TwoWayOrderOffer price))

deriving newtype instance Show p => Show (TwoWayOrder p)

deriving newtype instance Eq p => Eq (TwoWayOrder p)

deriving newtype instance Generic p => Generic (TwoWayOrder p)

-- deriving newtype instance (Swagger.ToSchema p, Generic p) => Swagger.ToSchema (TwoWayOrder p)
-- deriving newtype instance (Aeson.ToJSON p, Generic p) => Aeson.ToJSON (TwoWayOrder p)

data TwoWayOrderWays offer = TwoWayOrderWays
  { twoStraight :: !(TwoWayOrderAssetDetails offer),
    twoReverse :: !(TwoWayOrderAssetDetails (Maybe offer))
  }

deriving stock instance Show o => Show (TwoWayOrderWays o)

deriving stock instance Eq o => Eq (TwoWayOrderWays o)

deriving stock instance Generic o => Generic (TwoWayOrderWays o)

-- deriving anyclass instance (Swagger.ToSchema o, Generic o)
--   => Swagger.ToSchema (TwoWayOrderWays o)
-- deriving anyclass instance (Aeson.ToJSON o, Generic o)
--   => Aeson.ToJSON (TwoWayOrderWays o)

data TwoWayOrderAssetDetails offer = TwoWayOrderAssetDetails
  { -- | Asset class.
    twoAsset :: !AssetClass,
    -- | Available offer for this asset.
    twoOffer :: !offer
  }

deriving stock instance Show p => Show (TwoWayOrderAssetDetails p)

deriving stock instance Eq p => Eq (TwoWayOrderAssetDetails p)

deriving stock instance Generic p => Generic (TwoWayOrderAssetDetails p)

-- deriving anyclass instance (Swagger.ToSchema p, Generic p)
--   => Swagger.ToSchema (TwoWayOrderAssetDetails p)
-- deriving anyclass instance (Aeson.ToJSON p, Generic p)
--   => Aeson.ToJSON (TwoWayOrderAssetDetails p)

data TwoWayOrderOffer price = TwoWayOrderOffer
  { twoAmount :: !Integer,
    twoPrice :: !price
  }

deriving stock instance Show p => Show (TwoWayOrderOffer p)

deriving stock instance Eq p => Eq (TwoWayOrderOffer p)

deriving stock instance Generic p => Generic (TwoWayOrderOffer p)

-- deriving anyclass instance (Swagger.ToSchema p, Generic p)
--   => Swagger.ToSchema (TwoWayOrderOffer p)
-- deriving anyclass instance (Aeson.ToJSON p, Generic p)
--   => Aeson.ToJSON (TwoWayOrderOffer p)
