{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.OnChain.Core.Common.DEX.PartialOrder (PartialOrderFeeOutput (..), PartialOrderContainedFee (..), PartialOrderDatum (..), PartialOrderAction (..)) where

import GHC.Generics (Generic)
import GeniusYield.OnChain.Core.Common.LedgerExports.Common
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

-- | Representation of total fees contained in the order.
data PartialOrderContainedFee = PartialOrderContainedFee
  { -- | Fees explicitly charged in lovelaces, like flat lovelace fee collected from maker and taker(s).
    pocfLovelaces :: Integer,
    -- | Fees explicitly collected as percentage of offered tokens from maker.
    pocfOfferedTokens :: Integer,
    -- | Fees explicitly collected as percentage of asked tokens from taker.
    pocfAskedTokens :: Integer
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderContainedFee

instance Semigroup PartialOrderContainedFee where
  (<>) a b =
    PartialOrderContainedFee
      { pocfLovelaces = pocfLovelaces a + pocfLovelaces b,
        pocfOfferedTokens = pocfOfferedTokens a + pocfOfferedTokens b,
        pocfAskedTokens = pocfAskedTokens a + pocfAskedTokens b
      }

instance Monoid PartialOrderContainedFee where mempty = PartialOrderContainedFee 0 0 0

-- | Datum of the fee output.
data PartialOrderFeeOutput = PartialOrderFeeOutput
  { -- | Map, mapping order being consumed to the collected fees.
    pofdMentionedFees :: PlutusTx.Map TxOutRef Value,
    -- | Value reserved in this UTxO which is not to be considered as fees.
    pofdReservedValue :: Value,
    -- | If not @Nothing@, it mentions the UTxO being consumed, whose value is used to provide for UTxOs minimum ada requirement.
    pofdSpentUTxORef :: Maybe TxOutRef
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderFeeOutput

-- | Datum specifying a partial order.
data PartialOrderDatum = PartialOrderDatum
  { -- | Public key hash of the owner. Order cancellations must be signed by this.
    podOwnerKey :: PubKeyHash,
    -- | Address of the owner. Payments must be made to this address.
    podOwnerAddr :: Address,
    -- | The asset being offered.
    podOfferedAsset :: AssetClass,
    -- | Original number of units being offered. Initially, this would be same as `podOfferedAmount`.
    podOfferedOriginalAmount :: Integer,
    -- | The number of units being offered.
    podOfferedAmount :: Integer,
    -- | The asset being asked for as payment.
    podAskedAsset :: AssetClass,
    -- | The price for one unit of the offered asset.
    podPrice :: PlutusTx.Rational,
    -- | Token name of the NFT identifying this order.
    podNFT :: TokenName,
    -- | The time when the order can earliest be filled (optional).
    podStart :: Maybe POSIXTime,
    -- | The time when the order can latest be filled (optional).
    podEnd :: Maybe POSIXTime,
    -- | Number of partial fills order has undergone, initially would be 0.
    podPartialFills :: Integer,
    -- | Flat fee (in lovelace) paid by the maker.
    podMakerLovelaceFlatFee :: Integer,
    -- | Flat fee (in lovelace) paid by the taker.
    podTakerLovelaceFlatFee :: Integer,
    -- | Total fees contained in the order.
    podContainedFee :: PartialOrderContainedFee,
    -- | Payment (in asked asset) contained in the order.
    podContainedPayment :: Integer
  }
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''PartialOrderDatum

data PartialOrderAction
  = PartialCancel
  | PartialFill Integer
  | CompleteFill
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed ''PartialOrderAction [('PartialCancel, 0), ('PartialFill, 1), ('CompleteFill, 2)]
