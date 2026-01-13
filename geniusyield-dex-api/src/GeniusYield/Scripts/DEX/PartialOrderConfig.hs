{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GeniusYield.Scripts.DEX.PartialOrderConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Scripts.DEX.PartialOrderConfig (
  -- * Reexport version and related utilities
  POCVersion (..),
  SingPOCVersion (..),
  toSingPOCVersion,
  fromSingPOCVersion,
  SingPOCVersionI (..),
  SomeSingPOCVersion (..),
  withSomeSingPOCVersion,

  -- * Validator
  partialOrderConfigValidator,
  partialOrderConfigValidatorHash,
  partialOrderConfigPlutusAddr,

  -- * Datum
  PartialOrderConfigDatumF (..),
  PartialOrderConfigDatum,
) where

import GHC.Generics (Generic)
import GeniusYield.Scripts.DEX.PartialOrderConfig.OnChain qualified as OnChain
import GeniusYield.Scripts.DEX.Version
import GeniusYield.Scripts.Internal
import GeniusYield.Types
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx (
  BuiltinData,
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
 )
import Ply ((#))

data PartialOrderConfigDatumF addr = PartialOrderConfigDatum
  { -- | Public key hashes of the potential signatories.
    pocdSignatories :: ![GYPubKeyHash],
    -- | Number of required signatures.
    pocdReqSignatories :: !Integer,
    -- | Minting Policy Id of the partial order Nft.
    pocdNftSymbol :: !GYMintingPolicyId,
    -- | Address to which fees are paid.
    pocdFeeAddr :: !addr,
    -- | Flat fee (in lovelace) paid by the maker.
    pocdMakerFeeFlat :: !Integer,
    -- | Proportional fee (in the offered token) paid by the maker.
    pocdMakerFeeRatio :: !GYRational,
    -- | Flat fee (in lovelace) paid by the taker.
    pocdTakerFee :: !Integer,
    -- | Minimum required deposit (in lovelace).
    pocdMinDeposit :: !Integer
  }
  deriving (Functor, Generic, Show)

type PartialOrderConfigDatum = PartialOrderConfigDatumF GYAddress

instance ToData (PartialOrderConfigDatumF Plutus.Address) where
  toBuiltinData :: PartialOrderConfigDatumF Plutus.Address -> BuiltinData
  toBuiltinData PartialOrderConfigDatum {..} =
    toBuiltinData
      OnChain.PartialOrderConfigDatum
        { OnChain.pocdSignatories = pubKeyHashToPlutus <$> pocdSignatories,
          OnChain.pocdReqSignatories = pocdReqSignatories,
          OnChain.pocdNftSymbol = mintingPolicyIdToCurrencySymbol pocdNftSymbol,
          OnChain.pocdFeeAddr = pocdFeeAddr,
          OnChain.pocdMakerFeeFlat = pocdMakerFeeFlat,
          OnChain.pocdMakerFeeRatio = rationalToPlutus pocdMakerFeeRatio,
          OnChain.pocdTakerFee = pocdTakerFee,
          OnChain.pocdMinDeposit = pocdMinDeposit
        }

instance ToData PartialOrderConfigDatum where
  toBuiltinData :: PartialOrderConfigDatum -> BuiltinData
  toBuiltinData = toBuiltinData . fmap addressToPlutus

instance FromData (PartialOrderConfigDatumF Plutus.Address) where
  fromBuiltinData :: BuiltinData -> Maybe (PartialOrderConfigDatumF Plutus.Address)
  fromBuiltinData d = do
    OnChain.PartialOrderConfigDatum {..} <- fromBuiltinData d
    signatories <- fromEither $ mapM pubKeyHashFromPlutus pocdSignatories
    nftSymbol <- fromEither $ mintingPolicyIdFromCurrencySymbol pocdNftSymbol
    pure
      PartialOrderConfigDatum
        { pocdSignatories = signatories,
          pocdReqSignatories = pocdReqSignatories,
          pocdNftSymbol = nftSymbol,
          pocdFeeAddr = pocdFeeAddr,
          pocdMakerFeeFlat = pocdMakerFeeFlat,
          pocdMakerFeeRatio = rationalFromPlutus pocdMakerFeeRatio,
          pocdTakerFee = pocdTakerFee,
          pocdMinDeposit = pocdMinDeposit
        }
   where
    fromEither :: Either e a -> Maybe a
    fromEither = either (const Nothing) Just

partialOrderConfigValidatorHash
  :: GYCompiledScriptsRaw
  -> POCVersion
  -> GYAssetClass
  -> GYScriptHash
partialOrderConfigValidatorHash gycs pocVersion = validatorHash . partialOrderConfigValidator gycs pocVersion

partialOrderConfigValidator :: GYCompiledScriptsRaw -> POCVersion -> GYAssetClass -> GYScript PlutusV2
partialOrderConfigValidator GYCompiledScriptsRaw {gycsDEXPartialOrderConfig, gycsDEXPartialOrderConfigV1AppliedPreprod, gycsDEXPartialOrderConfigV1AppliedMainnet} pocVersion ac =
  case pocVersion of
    POCVersion1 ->
      if
        | ac == "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.8309f9861928a55d37e84f6594b878941edce5e351f7904c2c63b559bde45c5c" -> gycsDEXPartialOrderConfigV1AppliedPreprod
        | ac == "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.4aff78908ef2dce98bfe435fb3fd2529747b1c4564dff5adebedf4e46d0fc63d" -> gycsDEXPartialOrderConfigV1AppliedMainnet
        | otherwise -> error "partialOrderConfigValidator not found"
    -- \| otherwise -> validatorFromPlutus . OnChain.originalPartialOrderConfigValidator . assetClassToPlutus $ ac
    POCVersion1_1 -> validatorFromPly $ gycsDEXPartialOrderConfig # assetClassToPlutus ac

partialOrderConfigPlutusAddr :: GYCompiledScriptsRaw -> POCVersion -> GYAssetClass -> Plutus.Address
partialOrderConfigPlutusAddr gycs pocVersion ac = addressToPlutus $ addressFromValidator GYMainnet $ partialOrderConfigValidator gycs pocVersion ac
