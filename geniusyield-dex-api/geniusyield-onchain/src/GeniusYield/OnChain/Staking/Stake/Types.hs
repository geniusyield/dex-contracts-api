{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GeniusYield.OnChain.Staking.Stake.Types (PStakeDatum (..)) where

import Plutarch.Api.V1
import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

-- | 'PStakeDatum' is the plutarch level type for 'StakeDatum'.
newtype PStakeDatum (s :: S)
  = PStakeDatum
      ( Term
          s
          ( PDataRecord
              '[ "ownerKey" ':= PPubKeyHash,
                 "ownerAddr" ':= PAddress,
                 "lockedUntil" ':= PMaybeData PPOSIXTime
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PDataFields, PEq, PIsData, PlutusType)

instance DerivePlutusType PStakeDatum where type DPTStrat _ = PlutusTypeData
