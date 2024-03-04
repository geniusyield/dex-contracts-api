{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : GeniusYield.Scripts.Dex.PartialOrderConfig.Internal
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Scripts.Dex.PartialOrderConfig.Internal (
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
    pocdSignatories ∷ [PubKeyHash],
    -- | Number of required signatures.
    pocdReqSignatories ∷ Integer,
    -- | Currency symbol of the partial order Nft.
    pocdNftSymbol ∷ CurrencySymbol,
    -- | Address to which fees are paid.
    pocdFeeAddr ∷ Address,
    -- | Flat fee (in lovelace) paid by the maker.
    pocdMakerFeeFlat ∷ Integer,
    -- | Proportional fee (in the offered token) paid by the maker.
    pocdMakerFeeRatio ∷ Rational,
    -- | Flat fee (in lovelace) paid by the taker.
    pocdTakerFee ∷ Integer,
    -- | Minimum required deposit (in lovelace).
    pocdMinDeposit ∷ Integer
  }
  deriving (Generic, P.Show)

PlutusTx.unstableMakeIsData ''PartialOrderConfigDatum
