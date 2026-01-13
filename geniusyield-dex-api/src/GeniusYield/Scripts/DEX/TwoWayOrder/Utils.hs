{-# LANGUAGE LambdaCase #-}

module GeniusYield.Scripts.DEX.TwoWayOrder.Utils (
  mkTwoWayOrderFixedDatum,
  mkTwoWayOrderFixedDatumTwoWay,
  mkTwoWayOrderDeltaDatumOneWay,
  mkTwoWayOrderDeltaDatumTwoWay,
) where

import GeniusYield.Scripts.DEX.TwoWayOrder
import GeniusYield.Types
import PlutusLedgerApi.V1 (Address (..), Credential (..), POSIXTime (..), PubKeyHash (..), ScriptHash (..), StakingCredential (..))
import PlutusLedgerApi.V1.Value qualified as Ledger
import PlutusTx.Builtins (toBuiltin)
import PlutusTx.Ratio qualified as Tx

-- Internal core builder to avoid duplication between one-way and two-way fixed-price datums.
mkTwoWayOrderFixedDatumCore
  :: GYPubKeyHash
  -> GYAddress
  -> GYTokenName
  -> (GYAssetClass, Integer)
  -> (GYAssetClass, Integer)
  -> GYRational
  -> Maybe GYRational
  -> Maybe GYTime
  -> Maybe GYTime
  -> Integer
  -> GYRational
  -> GYRational
  -> Integer
  -> BPgeniusyield_dex_v2_types_order_OrderDatum
mkTwoWayOrderFixedDatumCore pkh ownerAddr nftName (aAC, aAmt) (bAC, bAmt) straight mReverse start end takerFlat takerRatio makerRatio freshnessSeconds =
  BPgeniusyield_dex_v2_types_order_OrderDatum0OrderDatum
    ownerMultisig
    bpPaymentAddr
    (Ledger.unTokenName $ tokenNameToPlutus nftName)
    (bpAssetDetails aAC aAmt)
    (bpAssetDetails bAC bAmt)
    bpPrice
    (bpTime start)
    (bpTime end)
    takerFlat
    (rationalFromPlutus' $ rationalToPlutus takerRatio)
    (rationalFromPlutus' $ rationalToPlutus makerRatio)
    freshnessSeconds
 where
  ownerMultisig =
    BPgeniusyield_dex_v2_types_multisig_MultisigScript0MultisigScript
      [getPubKeyHash $ pubKeyHashToPlutus pkh]
      []
  bpStakeCred m = case m of
    Nothing -> BPOption_StakeCredential1None
    Just (StakingHash cr) -> BPOption_StakeCredential0Some $ BPStakeCredential0Inline (credentialToCardanoAddress cr)
    Just (StakingPtr x y z) -> BPOption_StakeCredential0Some $ BPStakeCredential1Pointer x y z
  bpPaymentAddr =
    let Address cred mStak = addressToPlutus ownerAddr
     in BPcardano_address_Address0Address (credentialToBPPayment cred) (bpStakeCred mStak)
  bpAssetDetails ac amt =
    let Ledger.AssetClass (Ledger.CurrencySymbol polic, Ledger.TokenName assetName) = assetClassToPlutus ac
     in BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails
          (BPgeniusyield_dex_v2_types_assets_AssetClass0AssetClass polic assetName)
          amt
  bpPrice = case mReverse of
    Nothing -> BPgeniusyield_dex_v2_types_order_Price0Fixed (rationalFromPlutus' $ rationalToPlutus straight) BPOption_geniusyield_dex_v2_types_rational_Rational1None
    Just r -> BPgeniusyield_dex_v2_types_order_Price0Fixed (rationalFromPlutus' $ rationalToPlutus straight) (BPOption_geniusyield_dex_v2_types_rational_Rational0Some $ rationalFromPlutus' $ rationalToPlutus r)
  bpTime = maybe BPOption_Int1None (BPOption_Int0Some . getPOSIXTime . timeToPlutus)
  rationalFromPlutus' :: Tx.Rational -> BPgeniusyield_dex_v2_types_rational_Rational
  rationalFromPlutus' r = BPgeniusyield_dex_v2_types_rational_Rational0Rational (Tx.numerator r) (Tx.denominator r)
  credentialToBPPayment :: Credential -> BPPaymentCredential
  credentialToBPPayment = \case
    PubKeyCredential (PubKeyHash vkh) -> BPPaymentCredential0VerificationKey vkh
    ScriptCredential (ScriptHash sh) -> BPPaymentCredential1Script sh
  credentialToCardanoAddress :: Credential -> BPcardano_address_Credential
  credentialToCardanoAddress = \case
    PubKeyCredential (PubKeyHash vkh) -> BPcardano_address_Credential0VerificationKey vkh
    ScriptCredential (ScriptHash sh) -> BPcardano_address_Credential1Script sh

-- | Build blueprint OrderDatum for a fixed-price ONE-WAY TWO order
mkTwoWayOrderFixedDatum
  :: GYPubKeyHash
  -> GYAddress
  -> GYTokenName
  -> (GYAssetClass, Integer)
  -> GYAssetClass
  -> GYRational
  -> Maybe GYTime
  -> Maybe GYTime
  -> Integer
  -> GYRational
  -> GYRational
  -> Integer
  -> BPgeniusyield_dex_v2_types_order_OrderDatum
mkTwoWayOrderFixedDatum pkh ownerAddr nftName (offerAC, offerAmt') priceAC price start end takerFlat takerRatio makerRatio freshnessSeconds =
  mkTwoWayOrderFixedDatumCore pkh ownerAddr nftName (offerAC, offerAmt') (priceAC, 0) price Nothing start end takerFlat takerRatio makerRatio freshnessSeconds

-- (duplicate one-way builder removed; use core + thin wrapper above)

{- | Build blueprint OrderDatum for a fixed-price TWO order that contains
     both tokens ("true two-way"). Reverse price is set to the reciprocal
     of straight price.
-}
mkTwoWayOrderFixedDatumTwoWay
  :: GYPubKeyHash
  -> GYAddress
  -> GYTokenName
  -> (GYAssetClass, Integer)
  -> (GYAssetClass, Integer)
  -> GYRational
  -> GYRational
  -> Maybe GYTime
  -> Maybe GYTime
  -> Integer
  -> GYRational
  -> GYRational
  -> Integer
  -> BPgeniusyield_dex_v2_types_order_OrderDatum
mkTwoWayOrderFixedDatumTwoWay pkh ownerAddr nftName (offerAC, offerAmt') (revAC, revAmt') price revPrice start end takerFlat takerRatio makerRatio freshnessSeconds =
  mkTwoWayOrderFixedDatumCore pkh ownerAddr nftName (offerAC, offerAmt') (revAC, revAmt') price (Just revPrice) start end takerFlat takerRatio makerRatio freshnessSeconds

-- | Build blueprint OrderDatum for a relative-priced (DeltaOracle) ONE-WAY TWO order
mkTwoWayOrderDeltaDatumOneWay
  :: GYPubKeyHash
  -> GYAddress
  -> GYTokenName
  -> (GYAssetClass, Integer)
  -- ^ offered asset (a) and amount
  -> GYAssetClass
  -- ^ asked asset (b)
  -> GYPaymentVerificationKey
  -- ^ oracle key
  -> GYRational
  -- ^ offset
  -> GYRational
  -- ^ spread
  -> Maybe GYTime
  -> Maybe GYTime
  -> Integer
  -- ^ taker flat fee (lovelace)
  -> GYRational
  -- ^ taker percent ratio
  -> GYRational
  -- ^ maker percent ratio
  -> Integer
  -- ^ oracle freshness seconds
  -> BPgeniusyield_dex_v2_types_order_OrderDatum
mkTwoWayOrderDeltaDatumOneWay pkh ownerAddr nftName (offerAC, offerAmt') priceAC oracleKey offset spread start end takerFlat takerRatio makerRatio freshnessSeconds =
  BPgeniusyield_dex_v2_types_order_OrderDatum0OrderDatum
    ownerMultisig
    bpPaymentAddr
    (Ledger.unTokenName $ tokenNameToPlutus nftName)
    (bpAssetDetails offerAC offerAmt')
    (bpAssetDetails priceAC 0)
    bpPrice
    (bpTime start)
    (bpTime end)
    takerFlat
    (rationalFromPlutus' $ rationalToPlutus takerRatio)
    (rationalFromPlutus' $ rationalToPlutus makerRatio)
    freshnessSeconds
 where
  ownerMultisig =
    BPgeniusyield_dex_v2_types_multisig_MultisigScript0MultisigScript
      [getPubKeyHash $ pubKeyHashToPlutus pkh]
      []
  bpStakeCred m = case m of
    Nothing -> BPOption_StakeCredential1None
    Just (StakingHash cr) -> BPOption_StakeCredential0Some $ BPStakeCredential0Inline (credentialToCardanoAddress cr)
    Just (StakingPtr x y z) -> BPOption_StakeCredential0Some $ BPStakeCredential1Pointer x y z
  bpPaymentAddr =
    let Address cred mStak = addressToPlutus ownerAddr
     in BPcardano_address_Address0Address (credentialToBPPayment cred) (bpStakeCred mStak)
  bpAssetDetails ac amt =
    let Ledger.AssetClass (Ledger.CurrencySymbol polic, Ledger.TokenName assetName) = assetClassToPlutus ac
     in BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails
          (BPgeniusyield_dex_v2_types_assets_AssetClass0AssetClass polic assetName)
          amt
  bpPrice =
    let
      okBytes = toBuiltin (paymentVerificationKeyRawBytes oracleKey)
      rOff = rationalFromPlutus' $ rationalToPlutus offset
      rSpr = rationalFromPlutus' $ rationalToPlutus spread
      stra = BPgeniusyield_dex_v2_types_order_PriceDelta0PriceDelta rOff rSpr
     in
      BPgeniusyield_dex_v2_types_order_Price1DeltaOracle okBytes stra BPOption_geniusyield_dex_v2_types_order_PriceDelta1None
  bpTime = maybe BPOption_Int1None (BPOption_Int0Some . getPOSIXTime . timeToPlutus)
  rationalFromPlutus' :: Tx.Rational -> BPgeniusyield_dex_v2_types_rational_Rational
  rationalFromPlutus' r = BPgeniusyield_dex_v2_types_rational_Rational0Rational (Tx.numerator r) (Tx.denominator r)
  credentialToBPPayment :: Credential -> BPPaymentCredential
  credentialToBPPayment = \case
    PubKeyCredential (PubKeyHash vkh) -> BPPaymentCredential0VerificationKey vkh
    ScriptCredential (ScriptHash sh) -> BPPaymentCredential1Script sh
  credentialToCardanoAddress :: Credential -> BPcardano_address_Credential
  credentialToCardanoAddress = \case
    PubKeyCredential (PubKeyHash vkh) -> BPcardano_address_Credential0VerificationKey vkh
    ScriptCredential (ScriptHash sh) -> BPcardano_address_Credential1Script sh

-- | Build blueprint OrderDatum for a relative-priced (DeltaOracle) TRUE two-way TWO order
mkTwoWayOrderDeltaDatumTwoWay
  :: GYPubKeyHash
  -> GYAddress
  -> GYTokenName
  -> (GYAssetClass, Integer)
  -- ^ offered asset (a) and amount
  -> (GYAssetClass, Integer)
  -- ^ reverse asset (b) and amount
  -> GYPaymentVerificationKey
  -- ^ oracle key
  -> GYRational
  -- ^ straight offset
  -> GYRational
  -- ^ straight spread
  -> Maybe (GYRational, GYRational)
  -- ^ optional reverse (offset, spread). When Nothing, use the same delta as straight.
  -> Maybe GYTime
  -> Maybe GYTime
  -> Integer
  -- ^ taker flat fee (lovelace)
  -> GYRational
  -- ^ taker percent ratio
  -> GYRational
  -- ^ maker percent ratio
  -> Integer
  -- ^ oracle freshness seconds
  -> BPgeniusyield_dex_v2_types_order_OrderDatum
mkTwoWayOrderDeltaDatumTwoWay pkh ownerAddr nftName (offerAC, offerAmt') (revAC, revAmt') oracleKey offS sprS mRev start end takerFlat takerRatio makerRatio freshnessSeconds =
  BPgeniusyield_dex_v2_types_order_OrderDatum0OrderDatum
    ownerMultisig
    bpPaymentAddr
    (Ledger.unTokenName $ tokenNameToPlutus nftName)
    (bpAssetDetails offerAC offerAmt')
    (bpAssetDetails revAC revAmt')
    bpPrice
    (bpTime start)
    (bpTime end)
    takerFlat
    (rationalFromPlutus' $ rationalToPlutus takerRatio)
    (rationalFromPlutus' $ rationalToPlutus makerRatio)
    freshnessSeconds
 where
  ownerMultisig =
    BPgeniusyield_dex_v2_types_multisig_MultisigScript0MultisigScript
      [getPubKeyHash $ pubKeyHashToPlutus pkh]
      []
  bpStakeCred m = case m of
    Nothing -> BPOption_StakeCredential1None
    Just (StakingHash cr) -> BPOption_StakeCredential0Some $ BPStakeCredential0Inline (credentialToCardanoAddress cr)
    Just (StakingPtr x y z) -> BPOption_StakeCredential0Some $ BPStakeCredential1Pointer x y z
  bpPaymentAddr =
    let Address cred mStak = addressToPlutus ownerAddr
     in BPcardano_address_Address0Address (credentialToBPPayment cred) (bpStakeCred mStak)
  bpAssetDetails ac amt =
    let Ledger.AssetClass (Ledger.CurrencySymbol polic, Ledger.TokenName assetName) = assetClassToPlutus ac
     in BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails
          (BPgeniusyield_dex_v2_types_assets_AssetClass0AssetClass polic assetName)
          amt
  bpPrice =
    let
      okBytes = toBuiltin (paymentVerificationKeyRawBytes oracleKey)
      rOffS = rationalFromPlutus' $ rationalToPlutus offS
      rSprS = rationalFromPlutus' $ rationalToPlutus sprS
      stra = BPgeniusyield_dex_v2_types_order_PriceDelta0PriceDelta rOffS rSprS
      rev =
        case mRev of
          Nothing ->
            BPOption_geniusyield_dex_v2_types_order_PriceDelta0Some stra
          Just (o, s) ->
            let
              rOffR = rationalFromPlutus' $ rationalToPlutus o
              rSprR = rationalFromPlutus' $ rationalToPlutus s
             in
              BPOption_geniusyield_dex_v2_types_order_PriceDelta0Some
                (BPgeniusyield_dex_v2_types_order_PriceDelta0PriceDelta rOffR rSprR)
     in
      BPgeniusyield_dex_v2_types_order_Price1DeltaOracle okBytes stra rev
  bpTime = maybe BPOption_Int1None (BPOption_Int0Some . getPOSIXTime . timeToPlutus)
  rationalFromPlutus' :: Tx.Rational -> BPgeniusyield_dex_v2_types_rational_Rational
  rationalFromPlutus' r = BPgeniusyield_dex_v2_types_rational_Rational0Rational (Tx.numerator r) (Tx.denominator r)
  credentialToBPPayment :: Credential -> BPPaymentCredential
  credentialToBPPayment = \case
    PubKeyCredential (PubKeyHash vkh) -> BPPaymentCredential0VerificationKey vkh
    ScriptCredential (ScriptHash sh) -> BPPaymentCredential1Script sh
  credentialToCardanoAddress :: Credential -> BPcardano_address_Credential
  credentialToCardanoAddress = \case
    PubKeyCredential (PubKeyHash vkh) -> BPcardano_address_Credential0VerificationKey vkh
    ScriptCredential (ScriptHash sh) -> BPcardano_address_Credential1Script sh
