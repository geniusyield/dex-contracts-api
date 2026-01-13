{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.Scripts.DEX.TwoWayOrder (
  -- * Validators
  twoWayOrderMintValidator,
  twoWayOrderSpendValidator,
  twoWayOrderFillValidator,
  twoWayOrderFillPublishValidator,
  twoWayOrderCancelValidator,
  twoWayOrderCancelPublishValidator,

  -- * Datum
  BPgeniusyield_dex_v2_types_order_OrderDatum (..),

  -- * Redeemer
  BPgeniusyield_dex_v2_types_order_OrderRedeemer (..),

  -- * Generated types & functions
  BPAssetName,
  BPByteArray,
  BPData,
  BPInt,
  BPMintRedeemer (..),
  BPPolicyId,
  BPcardano_address_Credential (..),
  BPScriptHash,
  BPVerificationKeyHash,
  BPgeniusyield_dex_v2_types_multisig_MultisigScript (..),
  BPcardano_address_Address (..),
  BPgeniusyield_dex_v2_types_order_AssetDetails (..),
  BPgeniusyield_dex_v2_types_order_Price (..),
  BPOption_Int (..),
  BPgeniusyield_dex_v2_types_rational_Rational (..),
  BPcardano_transaction_OutputReference (..),
  BPgeniusyield_dex_v2_types_assets_AssetClass (..),
  BPgeniusyield_dex_v2_types_order_OutputReferenceInt (..),
  BPPaymentCredential (..),
  BPOption_StakeCredential (..),
  BPList_VerificationKeyHash,
  BPList_ScriptHash,
  BPOption_geniusyield_dex_v2_types_rational_Rational (..),
  BPVerificationKey,
  BPgeniusyield_dex_v2_types_order_PriceDelta (..),
  BPOption_geniusyield_dex_v2_types_order_PriceDelta (..),
  BPSignature,
  BPgeniusyield_dex_v2_types_order_PriceTimestamp (..),
  BPStakeCredential (..),
  applyParamsToBPValidator_order_order_validator_stake_fill_withdraw,
  applyParamsToBPValidator_order_order_validator_stake_fill_else,
  applyParamsToBPValidator_order_order_validator_stake_cancel_withdraw,
  applyParamsToBPValidator_order_order_validator_stake_cancel_else,
  applyParamsToBPValidator_order_order_validator_spend_spend,
  applyParamsToBPValidator_order_order_validator_spend_else,
  applyParamsToBPValidator_order_order_validator_mint_mint,
  applyParamsToBPValidator_order_order_validator_mint_else,
  scriptFromBPSerialisedScript,
) where

import GeniusYield.Scripts.BlueprintTH (makeBPTypes, uponBPTypes)
import GeniusYield.Types (
  GYAssetClass (..),
  GYScript,
  PlutusVersion (..),
  mintingPolicyIdToCurrencySymbol,
  scriptPlutusHash,
  tokenNameToPlutus,
 )
import PlutusLedgerApi.V1.Scripts (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (unTokenName))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

$(makeBPTypes "geniusyield-onchain/compiled/DEX.TwoWayOrder.json")
$(uponBPTypes "geniusyield-onchain/compiled/DEX.TwoWayOrder.json")

-- Note: The above TH splices depend on the external JSON file.
-- Touchpoint to trigger recompilation on blueprint refresh.
-- Updated: 2025-10-12 (net-fee semantics refresh)

twoWayOrderMintValidator :: GYAssetClass -> GYScript PlutusV3
twoWayOrderMintValidator refNft =
  scriptFromBPSerialisedScript $
    applyParamsToBPValidator_order_order_validator_mint_mint
      (bpAssetClass refNft)
      (bpScriptHash $ twoWayOrderSpendValidator refNft)

twoWayOrderSpendValidator :: GYAssetClass -> GYScript PlutusV3
twoWayOrderSpendValidator refNft =
  scriptFromBPSerialisedScript $
    applyParamsToBPValidator_order_order_validator_spend_spend
      (fillCredential refNft)
      (cancelCredential refNft)

twoWayOrderFillValidator :: GYAssetClass -> GYScript PlutusV3
twoWayOrderFillValidator =
  scriptFromBPSerialisedScript . applyParamsToBPValidator_order_order_validator_stake_fill_withdraw . bpAssetClass

twoWayOrderFillPublishValidator :: GYAssetClass -> GYScript PlutusV3
twoWayOrderFillPublishValidator =
  scriptFromBPSerialisedScript . applyParamsToBPValidator_order_order_validator_stake_fill_publish . bpAssetClass

twoWayOrderCancelValidator :: GYAssetClass -> GYScript PlutusV3
twoWayOrderCancelValidator =
  scriptFromBPSerialisedScript . applyParamsToBPValidator_order_order_validator_stake_cancel_withdraw . bpAssetClass

twoWayOrderCancelPublishValidator :: GYAssetClass -> GYScript PlutusV3
twoWayOrderCancelPublishValidator =
  scriptFromBPSerialisedScript . applyParamsToBPValidator_order_order_validator_stake_cancel_publish . bpAssetClass

bpAssetClass :: GYAssetClass -> BPgeniusyield_dex_v2_types_assets_AssetClass
bpAssetClass GYLovelace = BPgeniusyield_dex_v2_types_assets_AssetClass0AssetClass "" ""
bpAssetClass (GYToken pid tn) = BPgeniusyield_dex_v2_types_assets_AssetClass0AssetClass pid' tn'
 where
  pid', tn' :: BuiltinByteString
  pid' = unCurrencySymbol $ mintingPolicyIdToCurrencySymbol pid
  tn' = unTokenName $ tokenNameToPlutus tn

bpScriptHash :: GYScript v -> BPScriptHash
bpScriptHash = getScriptHash . scriptPlutusHash

fillCredential, cancelCredential :: GYAssetClass -> BPcardano_address_Credential
fillCredential = BPcardano_address_Credential1Script . bpScriptHash . twoWayOrderFillValidator
cancelCredential = BPcardano_address_Credential1Script . bpScriptHash . twoWayOrderCancelValidator
