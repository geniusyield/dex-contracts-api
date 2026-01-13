{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.Scripts.DEX.TwoWayOrderConfig (
  twoWayOrderConfigValidator,
  TwoWayOrderConfigDatumF (..),
  TwoWayOrderConfigDatum,
) where

import GHC.Generics (type Generic)
import GeniusYield.Scripts.Internal
import GeniusYield.Types
import PlutusLedgerApi.V2 (
  Address,
  BuiltinData,
  CurrencySymbol,
  PubKeyHash,
  ScriptHash (..),
 )
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  unstableMakeIsData,
 )
import PlutusTx.Ratio qualified as P (Rational)
import Ply ((#))

data TwoWayOrderConfigDatumF addr = TwoWayOrderConfigDatum
  { -- | Public key hashes of the potential signatories.
    twocdSignatories :: ![GYPubKeyHash],
    -- | Number of required signatures.
    twocdReqSignatories :: !Integer,
    -- | Minting Policy Id of the two-way order Nft.
    twocdNftSymbol :: !GYMintingPolicyId,
    -- | Script to fill the order.
    twocdFillHash :: !GYScriptHash,
    -- | Script to cancel the order.
    twocdCancelHash :: !GYScriptHash,
    -- | Address to which fees are paid.
    twocdFeeAddr :: !addr,
    -- | Flat fee (in lovelace) paid by the maker.
    twocdMakerFeeFlat :: !Integer,
    -- | Proportional fee (in the offered token) paid by the maker.
    twocdMakerFeeRatio :: !GYRational,
    -- | Flat fee (in lovelace) paid by the taker.
    twocdTakerFeeFlat :: !Integer,
    -- | Proportional fee (in the offered token) paid by the taker.
    twocdTakerFeeRatio :: !GYRational,
    -- | Maximum age (in seconds) for oracle prices accepted by fills.
    twocdOracleFreshnessSeconds :: !Integer,
    -- | Minimum required deposit (in lovelace).
    twocdMinDeposit :: !Integer
  }
  deriving (Functor, Generic, Show)

type TwoWayOrderConfigDatum = TwoWayOrderConfigDatumF GYAddress

-- TODO: refactor, we don't actually need TwoWayOrderConfigDatum'
data TwoWayOrderConfigDatum' = TwoWayOrderConfigDatum'
  { -- | Public key hashes of the potential signatories.
    twocd'Signatories :: ![PubKeyHash],
    -- | Number of required signatures.
    twocd'ReqSignatories :: !Integer,
    -- | Minting Policy Id of the two-way order Nft.
    twocd'NftSymbol :: !CurrencySymbol,
    -- | Script to fill the order.
    twocd'FillHash :: !ScriptHash,
    -- | Script to cancel the order.
    twocd'CancelHash :: !ScriptHash,
    -- | Address to which fees are paid.
    twocd'FeeAddr :: !Address,
    -- | Flat fee (in lovelace) paid by the maker.
    twocd'MakerFeeFlat :: !Integer,
    -- | Proportional fee (in the offered token) paid by the maker.
    twocd'MakerFeeRatio :: !P.Rational,
    -- | Flat fee (in lovelace) paid by the taker.
    twocd'TakerFeeFlat :: !Integer,
    -- | Proportional fee (in the offered token) paid by the taker.
    twocd'TakerFeeRatio :: !P.Rational,
    -- | Maximum age (in seconds) for oracle prices accepted by fills.
    twocd'OracleFreshnessSeconds :: !Integer,
    -- | Minimum required deposit (in lovelace).
    twocd'MinDeposit :: !Integer
  }
  deriving (Generic, Show)

unstableMakeIsData ''TwoWayOrderConfigDatum'

instance ToData (TwoWayOrderConfigDatumF Address) where
  toBuiltinData :: TwoWayOrderConfigDatumF Address -> BuiltinData
  toBuiltinData TwoWayOrderConfigDatum {..} =
    toBuiltinData
      TwoWayOrderConfigDatum'
        { twocd'Signatories = pubKeyHashToPlutus <$> twocdSignatories,
          twocd'ReqSignatories = twocdReqSignatories,
          twocd'NftSymbol = mintingPolicyIdToCurrencySymbol twocdNftSymbol,
          twocd'FillHash = scriptHashToPlutus twocdFillHash,
          twocd'CancelHash = scriptHashToPlutus twocdCancelHash,
          twocd'FeeAddr = twocdFeeAddr,
          twocd'MakerFeeFlat = twocdMakerFeeFlat,
          twocd'MakerFeeRatio = rationalToPlutus twocdMakerFeeRatio,
          twocd'TakerFeeFlat = twocdTakerFeeFlat,
          twocd'TakerFeeRatio = rationalToPlutus twocdTakerFeeRatio,
          twocd'OracleFreshnessSeconds = twocdOracleFreshnessSeconds,
          twocd'MinDeposit = twocdMinDeposit
        }

instance ToData TwoWayOrderConfigDatum where
  toBuiltinData :: TwoWayOrderConfigDatum -> BuiltinData
  toBuiltinData = toBuiltinData . fmap addressToPlutus

instance FromData (TwoWayOrderConfigDatumF Address) where
  fromBuiltinData :: BuiltinData -> Maybe (TwoWayOrderConfigDatumF Address)
  fromBuiltinData d = do
    TwoWayOrderConfigDatum' {..} <- fromBuiltinData d
    signatories <- fromEither $ mapM pubKeyHashFromPlutus twocd'Signatories
    nftSymbol <- fromEither $ mintingPolicyIdFromCurrencySymbol twocd'NftSymbol
    fillHash <- fromEither $ validatorHashFromPlutus twocd'FillHash
    cancelHash <- fromEither $ validatorHashFromPlutus twocd'CancelHash

    pure
      TwoWayOrderConfigDatum
        { twocdSignatories = signatories,
          twocdReqSignatories = twocd'ReqSignatories,
          twocdNftSymbol = nftSymbol,
          twocdFillHash = fillHash,
          twocdCancelHash = cancelHash,
          twocdFeeAddr = twocd'FeeAddr,
          twocdMakerFeeFlat = twocd'MakerFeeFlat,
          twocdMakerFeeRatio = rationalFromPlutus twocd'MakerFeeRatio,
          twocdTakerFeeFlat = twocd'TakerFeeFlat,
          twocdTakerFeeRatio = rationalFromPlutus twocd'TakerFeeRatio,
          twocdOracleFreshnessSeconds = twocd'OracleFreshnessSeconds,
          twocdMinDeposit = twocd'MinDeposit
        }
   where
    fromEither :: Either e a -> Maybe a
    fromEither = either (const Nothing) Just

twoWayOrderConfigValidator
  :: GYCompiledScriptsRaw -> GYAssetClass -> GYScript PlutusV2
twoWayOrderConfigValidator GYCompiledScriptsRaw {gycsDEXTwoWayOrderConfig} ac =
  validatorFromPly $ gycsDEXTwoWayOrderConfig # assetClassToPlutus ac
