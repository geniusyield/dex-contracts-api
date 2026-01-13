{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.Staking.Stake (StakeDatum (..)) where

import GHC.Generics (Generic)
import GeniusYield.OnChain.Core.Common.LedgerExports.Common
import PlutusTx qualified

-- | The datum for staked value. Contains owner address, owner key and (optionally) the earliest time of retrieval.
data StakeDatum = StakeDatum
  { -- | The owner key. To retrieve the funds, the owner must sign.
    sdOwnerKey :: !PubKeyHash,
    -- | The owner address. This has no impact on validation.
    sdOwnerAddr :: !Address,
    -- | If set, this denotes the earliest time when the funds can be retrieved.
    sdLockedUntil :: !(Maybe POSIXTime)
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''StakeDatum
