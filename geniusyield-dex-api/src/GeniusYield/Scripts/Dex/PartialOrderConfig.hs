{- |
Module      : GeniusYield.Scripts.Dex.PartialOrderConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Scripts.Dex.PartialOrderConfig (
  -- * Typeclass
  HasPartialOrderConfigAddr (..),

  -- * Datum
  PartialOrderConfigDatum (..),
  PartialOrderConfigInfoF (..),
  PartialOrderConfigInfo,
) where

import GHC.Generics (Generic)
import GeniusYield.Scripts.Dex.PartialOrderConfig.Internal (PartialOrderConfigDatum (..))
import GeniusYield.Scripts.Dex.Version (POCVersion)
import GeniusYield.Types
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx (
  BuiltinData,
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
 )

class HasPartialOrderConfigAddr a where
  getPartialOrderConfigAddr ∷ a → POCVersion → GYAddress

data PartialOrderConfigInfoF addr = PartialOrderConfigInfo
  { -- | Public key hashes of the potential signatories.
    pociSignatories ∷ ![GYPubKeyHash],
    -- | Number of required signatures.
    pociReqSignatories ∷ !Integer,
    -- | Minting Policy Id of the partial order Nft.
    pociNftSymbol ∷ !GYMintingPolicyId,
    -- | Address to which fees are paid.
    pociFeeAddr ∷ !addr,
    -- | Flat fee (in lovelace) paid by the maker.
    pociMakerFeeFlat ∷ !Integer,
    -- | Proportional fee (in the offered token) paid by the maker.
    pociMakerFeeRatio ∷ !GYRational,
    -- | Flat fee (in lovelace) paid by the taker.
    pociTakerFee ∷ !Integer,
    -- | Minimum required deposit (in lovelace).
    pociMinDeposit ∷ !Integer
  }
  deriving stock (Show, Generic, Functor)

type PartialOrderConfigInfo = PartialOrderConfigInfoF GYAddress

instance ToData (PartialOrderConfigInfoF Plutus.Address) where
  toBuiltinData ∷ PartialOrderConfigInfoF Plutus.Address → BuiltinData
  toBuiltinData PartialOrderConfigInfo {..} =
    toBuiltinData
      PartialOrderConfigDatum
        { pocdSignatories = pubKeyHashToPlutus <$> pociSignatories,
          pocdReqSignatories = pociReqSignatories,
          pocdNftSymbol = mintingPolicyIdToCurrencySymbol pociNftSymbol,
          pocdFeeAddr = pociFeeAddr,
          pocdMakerFeeFlat = pociMakerFeeFlat,
          pocdMakerFeeRatio = rationalToPlutus pociMakerFeeRatio,
          pocdTakerFee = pociTakerFee,
          pocdMinDeposit = pociMinDeposit
        }

instance ToData PartialOrderConfigInfo where
  toBuiltinData ∷ PartialOrderConfigInfo → BuiltinData
  toBuiltinData = toBuiltinData . fmap addressToPlutus

instance FromData (PartialOrderConfigInfoF Plutus.Address) where
  fromBuiltinData ∷ BuiltinData → Maybe (PartialOrderConfigInfoF Plutus.Address)
  fromBuiltinData d = do
    PartialOrderConfigDatum {..} ← fromBuiltinData d
    signatories ← fromEither $ mapM pubKeyHashFromPlutus pocdSignatories
    nftSymbol ← fromEither $ mintingPolicyIdFromCurrencySymbol pocdNftSymbol
    pure
      PartialOrderConfigInfo
        { pociSignatories = signatories,
          pociReqSignatories = pocdReqSignatories,
          pociNftSymbol = nftSymbol,
          pociFeeAddr = pocdFeeAddr,
          pociMakerFeeFlat = pocdMakerFeeFlat,
          pociMakerFeeRatio = rationalFromPlutus pocdMakerFeeRatio,
          pociTakerFee = pocdTakerFee,
          pociMinDeposit = pocdMinDeposit
        }
   where
    fromEither ∷ Either e a → Maybe a
    fromEither = either (const Nothing) Just
