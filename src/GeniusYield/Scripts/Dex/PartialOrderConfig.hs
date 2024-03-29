{- |
Module      : GeniusYield.Scripts.Dex.PartialOrderConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Scripts.Dex.PartialOrderConfig (
  -- * Typeclass
  HasPartialOrderConfigScript (..),
  HasPartialOrderConfigAddr (..),

  -- * Datum
  PartialOrderConfigDatum (..),
  PartialOrderConfigInfoF (..),
  PartialOrderConfigInfo,

  -- * Validator
  mkPartialOrderConfigValidator,

  -- * Address
  partialOrderConfigAddr,
  partialOrderConfigPlutusAddr,
) where

import GHC.Generics (Generic)
import GeniusYield.Scripts.Dex.PartialOrderConfig.Internal (PartialOrderConfigDatum (..))
import GeniusYield.Types
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx (
  BuiltinData,
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
 )
import PlutusTx qualified

class HasPartialOrderConfigScript a where
  getPartialOrderConfigValidator ∷ a → PlutusTx.CompiledCode (AssetClass → BuiltinData → BuiltinData → BuiltinData → ())

class HasPartialOrderConfigAddr a where
  getPartialOrderConfigAddr ∷ a → GYAddress

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

mkPartialOrderConfigValidator ∷ HasPartialOrderConfigScript a ⇒ a → GYAssetClass → GYValidator 'PlutusV2
mkPartialOrderConfigValidator script ac =
  let val = getPartialOrderConfigValidator script `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (assetClassToPlutus ac)
   in validatorFromPlutus val

partialOrderConfigAddr ∷ HasPartialOrderConfigScript a ⇒ a → GYNetworkId → GYAssetClass → GYAddress
partialOrderConfigAddr script nid ac =
  addressFromValidator nid $ mkPartialOrderConfigValidator script ac

partialOrderConfigPlutusAddr ∷ HasPartialOrderConfigScript a ⇒ a → GYAssetClass → Plutus.Address
partialOrderConfigPlutusAddr script ac = addressToPlutus $ partialOrderConfigAddr script GYMainnet ac
