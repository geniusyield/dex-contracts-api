{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GeniusYield.OnChain.DEX.TwoWayOrderConfig (
  mkTwoWayOrderConfigValidator,
) where

import GeniusYield.OnChain.Plutarch.Api (
  PAssetClass,
  pguardC,
  pguardC',
  phasSignatures,
  pletC,
  pletFieldsC,
  pmatchC,
 )
import GeniusYield.OnChain.Plutarch.Utils (
  pallUnique,
  pfindOwnInput,
  pgetContinuingOutputUsingNft,
  pparseDatum',
 )
import GeniusYield.OnChain.Plutarch.Value (passetClassValueOf, pvalTotalEntries)
import Plutarch (Term, pcon, plam, unTermCont, (#), type (:-->))
import Plutarch.Api.V1 (PAddress, PCurrencySymbol, PPubKeyHash, PScriptPurpose (PSpending))
import Plutarch.Api.V2 qualified as PV2
import Plutarch.DataRepr (PDataFields, pfield)
import Plutarch.Extra.RationalData
import Plutarch.List (pfind)
import Plutarch.Prelude (
  DerivePlutusType (..),
  Generic,
  PAsData,
  PBool (..),
  PBuiltinList,
  PData,
  PDataRecord,
  PEq ((#==)),
  PInteger,
  PIsData,
  PLabeledType ((:=)),
  PMaybe (..),
  PPartialOrd ((#<=)),
  PTryFrom,
  PUnit (..),
  PlutusType,
  PlutusTypeData,
  getField,
  pfromData,
  plength,
  pmatch,
 )

type TwoWayOrderConfigRec =
  '[ "twocdSignatories" ':= PBuiltinList (PAsData PPubKeyHash),
     "twocdReqSignatories" ':= PInteger,
     "twocdNftSymbol" ':= PCurrencySymbol,
     "twocdFillHash" ':= PV2.PScriptHash,
     "twocdCancelHash" ':= PV2.PScriptHash,
     "twocdFeeAddr" ':= PAddress,
     "twocdMakerFeeFlat" ':= PInteger,
     "twocdMakerFeeRatio" ':= PRationalData,
     "twocdTakerFeeFlat" ':= PInteger,
     "twocdTakerFeeRatio" ':= PRationalData,
     "twocdOracleFreshnessSeconds" ':= PInteger,
     "twocdMinDeposit" ':= PInteger
   ]

newtype PTwoWayOrderConfigDatum s
  = PTwoWayOrderConfigDatum (Term s (PDataRecord TwoWayOrderConfigRec))
  deriving stock (Generic)
  deriving anyclass (PDataFields, PEq, PIsData, PlutusType)

instance DerivePlutusType PTwoWayOrderConfigDatum where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PTwoWayOrderConfigDatum)

mkTwoWayOrderConfigValidator
  :: forall s
   . Term
       s
       ( PAssetClass
           :--> PTwoWayOrderConfigDatum
           :--> PUnit
           :--> PV2.PScriptContext
           :--> PUnit
       )
mkTwoWayOrderConfigValidator =
  plam $ \nftAC d _ ctx -> unTermCont $ do
    ctxFs <- pletFieldsC @["txInfo", "purpose"] ctx
    info <-
      pletFieldsC
        @[ "inputs",
           "outputs",
           "signatories",
           "datums"
         ]
        $ getField @"txInfo" ctxFs

    -- Find our own input, asserting spending validator.
    -- Knowing own input is important to find the continuing output, as it must be at same address.
    -- Additionally, we allow spending of an UTxO belonging to this validator if it lacks the required NFT.
    PSpending spRec <- pmatchC $ getField @"purpose" ctxFs
    ownRef <- pletC $ pfield @"_0" # spRec
    PJust ownInput <- pmatchC $ pfindOwnInput # getField @"inputs" info # ownRef
    ownInpUtxoFs <- pletFieldsC @["value", "address"] $ pfield @"resolved" # ownInput

    -- Succeed immediately if there is no NFT.
    pguardC' (pcon PUnit) $ passetClassValueOf # getField @"value" ownInpUtxoFs # nftAC #== 1

    dFs <-
      pletFieldsC
        @[ "twocdSignatories",
           "twocdReqSignatories",
           "twocdNftSymbol"
         ]
        d
    -- Assert multi-sig is correctly exercised.
    pguardC "missing signature(s)" $ phasSignatures # getField @"signatories" info # getField @"twocdSignatories" dFs # getField @"twocdReqSignatories" dFs

    -- Find continuing output with updated datum.
    outputs <- pletC $ getField @"outputs" info
    ownOutUtxo <- pletC $ pgetContinuingOutputUsingNft # getField @"address" ownInpUtxoFs # nftAC # outputs
    ownOutUtxoFs <- pletFieldsC @["value", "datum"] ownOutUtxo
    -- Continuing output does not have more than 10 tokens.
    pguardC "continuing output's value should have <= 10 tokens" $ pvalTotalEntries # getField @"value" ownOutUtxoFs #<= 10
    -- Assert new datum is of correct shape. Unlike PlutusTx, it checks whether credentials are of correct length, etc.
    newDatum <- pletC $ pfromData $ pparseDatum' @PTwoWayOrderConfigDatum # getField @"datum" ownOutUtxoFs # getField @"datums" info
    newDatumFs <-
      pletFieldsC
        @[ "twocdNftSymbol",
           "twocdMakerFeeFlat",
           "twocdMakerFeeRatio",
           "twocdTakerFeeFlat",
           "twocdTakerFeeRatio",
           "twocdOracleFreshnessSeconds",
           "twocdMinDeposit",
           "twocdReqSignatories",
           "twocdSignatories",
           "twocdFeeAddr",
           "twocdFillHash",
           "twocdCancelHash"
         ]
        newDatum

    -- Check the fields of new datum and assert that they are bounded.

    -- @twocdSignatories@ are unique and their number lies b/w 1 & 10 (inclusive).
    -- Note that it is possible to dissolve multi-sig by giving a single signatory for which no corresponding key is known.
    newSigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC $ getField @"twocdSignatories" newDatumFs
    pguardC "duplicate signatories" $ pallUnique # newSigs
    -- We are iterating over list of signatories twice (earlier when determining duplicates and now, to determine length) but performance is not a concern here.
    newSigsNum <- pletC $ plength # newSigs
    pguardC "too many signatories" $ newSigsNum #<= 10
    pguardC "non-positive signatories" $ 1 #<= newSigsNum

    -- @twocdReqSignatories@ is positive and not more than the number of signatories.
    newReqSigs :: Term _ PInteger <- pletC $ getField @"twocdReqSignatories" newDatumFs
    pguardC "non-positive number of required signatories" $ 1 #<= newReqSigs
    pguardC "too many required signatories" $ newReqSigs #<= newSigsNum

    -- @twocdNftSymbol@ is not altered.
    pguardC "twocdNftSymbol changed" $ getField @"twocdNftSymbol" dFs #== getField @"twocdNftSymbol" newDatumFs

    -- Even though we have checked the format of fee address when parsing the datum, but to be sure of any edges, we assert that an output is made to this address as part of this transaction.
    newFeeAddr <- pletC $ getField @"twocdFeeAddr" newDatumFs
    pguardC "not paid to fee address" $
      -- We are iterating over list of outputs twice (traversed earlier when finding continuing output) but performance is not a concern here.
      pmatch (pfind # plam (\output -> pfield @"address" # output #== newFeeAddr) # outputs) $
        \case
          PNothing -> pcon PFalse
          PJust _ -> pcon PTrue

    -- @twocdMakerFeeFlat@, @twocdTakerFee@ and @twocdMinDeposit@ are all non-negative and not more than 1000 ADA.
    let lovelaceThreshold = 1_000_000_000
    newMakerFeeFlat :: Term _ PInteger <- pletC $ getField @"twocdMakerFeeFlat" newDatumFs
    pguardC "negative flat maker fee" $ 0 #<= newMakerFeeFlat
    pguardC "high flat maker fee" $ newMakerFeeFlat #<= lovelaceThreshold
    newTakerFeeFlat :: Term _ PInteger <- pletC $ getField @"twocdTakerFeeFlat" newDatumFs
    pguardC "negative flat taker fee" $ 0 #<= newTakerFeeFlat
    pguardC "high flat taker fee" $ newTakerFeeFlat #<= lovelaceThreshold
    newMinDeposit :: Term _ PInteger <- pletC $ getField @"twocdMinDeposit" newDatumFs
    pguardC "negative min ada deposit" $ 0 #<= newMinDeposit
    pguardC "high min ada deposit" $ newMinDeposit #<= lovelaceThreshold

    -- @twocdMakerFeeRatio@ is non-negative and not more than 1.
    newMakerFeeRatio :: Term _ PRationalData <- pletC $ getField @"twocdMakerFeeRatio" newDatumFs
    pguardC "negative maker fee ratio" $ 0 #<= (pfield @"numerator" # newMakerFeeRatio :: Term _ PInteger)
    -- Module @Plutarch.Extra.RationalData@ does not export constructor for @PRationalData@, so comparison is performed using `prationalFromData`.
    pguardC "high maker fee ratio" $ prationalFromData # newMakerFeeRatio #<= 1

    -- @twocdTakerFeeRatio@ is non-negative and not more than 1.
    newTakerFeeRatio :: Term _ PRationalData <- pletC $ getField @"twocdTakerFeeRatio" newDatumFs
    pguardC "negative taker fee ratio" $ 0 #<= (pfield @"numerator" # newTakerFeeRatio :: Term _ PInteger)
    -- Module @Plutarch.Extra.RationalData@ does not export constructor for @PRationalData@, so comparison is performed using `prationalFromData`.
    pguardC "high taker fee ratio" $ prationalFromData # newTakerFeeRatio #<= 1

    -- @twocdOracleFreshnessSeconds@ is non-negative.
    newFreshness :: Term _ PInteger <- pletC $ getField @"twocdOracleFreshnessSeconds" newDatumFs
    pguardC "negative oracle freshness" $ 0 #<= newFreshness

    -- All good, we succeed.
    pure . pcon $ PUnit
