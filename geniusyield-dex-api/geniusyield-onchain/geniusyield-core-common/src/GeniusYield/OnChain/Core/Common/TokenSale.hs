{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.TokenSale (
  minTokenSaleDeposit,
  TokenSaleParams (..),
  OrderDatum (..),
  OrderAction (..),
) where

import GHC.Generics (Generic)
import GeniusYield.OnChain.Core.Common.LedgerExports.Common
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx

{- | The minimum deposit (in lovelace) to place with an order.
This deposit will be ignored in price- and fee-calculations and will be given back to the owner in the end.
Its purpose is to make sure Cardano ledger requirements for minimal UTxO's are met.
-}
minTokenSaleDeposit :: Num a => a
minTokenSaleDeposit = 2_000_000

-- | Parametereizes a token sale.
data TokenSaleParams = TokenSaleParams
  { -- | Start time for buying the token.
    tspBeginSale :: !POSIXTime,
    -- | Deadline for buying the token.
    tspEndSale :: !POSIXTime,
    -- | Deadline for distributing the bought tokens.
    tspEndDistribution :: !POSIXTime,
    -- | The token on sale.
    tspToken :: !AssetClass,
    -- | Price for one token in lovelace.
    tspPrice :: !PlutusTx.Rational,
    -- | The token seller.
    tspSellerKey :: !PubKeyHash,
    -- | The minimal token allocation.
    tspMinAllocation :: !PlutusTx.Integer,
    -- | The fees for the platform provider.
    tspFee :: !PlutusTx.Rational,
    -- | The address where the fees must be sent to
    tspFeeAddress :: !Address
  }
  deriving (Generic)

PlutusTx.unstableMakeIsData ''TokenSaleParams

--
----------------------------------------------------------------------

{- | Datum specifying an order, given by the "owner" (who will reclaim the order)
    and the address to send the tokens to.
-}
data OrderDatum = OrderDatum
  { odOwnerKey :: !PubKeyHash,
    odOwnerAddr :: !Address
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''OrderDatum

--
----------------------------------------------------------------------

{- | Possible ways to "unlock" an order: It can either be /filled/ by the seller, sending tokens to the owner during the distribution phase,
or /cancelled/ by the owner after the distribution phase.
-}
data OrderAction
  = -- | Cancel the order.
    Cancel
  | -- | Fill the order.
    Fill
  deriving (Show)

PlutusTx.makeIsDataIndexed ''OrderAction [('Cancel, 0), ('Fill, 1)]
