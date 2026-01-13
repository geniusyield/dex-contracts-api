{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GeniusYield.Scripts.DEX.PartialOrderConfig.OnChain (
  PartialOrderConfigDatum (..),
) where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (Address, PubKeyHash)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx qualified
import PlutusTx.Prelude (Integer)
import PlutusTx.Ratio (Rational)
import Prelude qualified as P

data PartialOrderConfigDatum = PartialOrderConfigDatum
  { -- | Public key hashes of the potential signatories.
    pocdSignatories :: [PubKeyHash],
    -- | Number of required signatures.
    pocdReqSignatories :: Integer,
    -- | Currency symbol of the partial order Nft.
    pocdNftSymbol :: CurrencySymbol,
    -- | Address to which fees are paid.
    pocdFeeAddr :: Address,
    -- | Flat fee (in lovelace) paid by the maker.
    pocdMakerFeeFlat :: Integer,
    -- | Proportional fee (in the offered token) paid by the maker.
    pocdMakerFeeRatio :: Rational,
    -- | Flat fee (in lovelace) paid by the taker.
    pocdTakerFee :: Integer,
    -- | Minimum required deposit (in lovelace).
    pocdMinDeposit :: Integer
  }
  deriving (Generic, P.Show)

PlutusTx.unstableMakeIsData ''PartialOrderConfigDatum
