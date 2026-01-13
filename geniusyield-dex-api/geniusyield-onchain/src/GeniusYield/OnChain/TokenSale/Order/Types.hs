{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GeniusYield.OnChain.TokenSale.Order.Types (
  PTokenSaleParams (..),
  POrderDatum (..),
  POrderAction (..),
) where

import GeniusYield.OnChain.Plutarch.Types (PAssetClass (..))
import Plutarch.Api.V1
import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

----------------------------------------------------------------------

-- | 'PTokenSaleParams' is the plutarch level type for 'TokenSaleParams'.
newtype PTokenSaleParams (s :: S)
  = PTokenSaleParams
      ( Term
          s
          ( PDataRecord
              '[ "beginSale" ':= PPOSIXTime,
                 "endSale" ':= PPOSIXTime,
                 "endDistribution" ':= PPOSIXTime,
                 "token" ':= PAssetClass,
                 "price" ':= PRational,
                 "sellerKey" ':= PPubKeyHash,
                 "minAllocation" ':= PInteger,
                 "fee" ':= PRational,
                 "feeAddress" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PDataFields, PEq, PIsData, PlutusType)

instance DerivePlutusType PTokenSaleParams where type DPTStrat _ = PlutusTypeData

----------------------------------------------------------------------

-- | 'POrderDatum' is the plutarch level type for 'OrderDatum'.
newtype POrderDatum (s :: S)
  = POrderDatum
      ( Term
          s
          ( PDataRecord
              '[ "ownerKey" ':= PPubKeyHash,
                 "ownerAddr" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PDataFields, PEq, PIsData, PlutusType)

instance DerivePlutusType POrderDatum where type DPTStrat _ = PlutusTypeData

----------------------------------------------------------------------

-- | 'POrderAction' is the plutarch level type for 'OrderAction'.
data POrderAction (s :: S)
  = PCancel (Term s (PDataRecord '[]))
  | PFill (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PEq, PIsData, PlutusType)

instance DerivePlutusType POrderAction where type DPTStrat _ = PlutusTypeData
