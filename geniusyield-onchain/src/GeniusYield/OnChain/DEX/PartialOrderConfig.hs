{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GeniusYield.OnChain.DEX.PartialOrderConfig (mkPartialOrderConfigValidator) where

import           GeniusYield.OnChain.DEX.PartialOrder.Types (PPartialOrderConfigDatum)
import           GeniusYield.OnChain.Plutarch.Api           (PAssetClass, pguardC, pguardC', phasSignatures, pletC,
                                                             pletFieldsC, pmatchC)
import           GeniusYield.OnChain.Plutarch.Utils         (pallUnique, pfindOwnInput, pgetContinuingOutputUsingNft,
                                                             pparseDatum')
import           GeniusYield.OnChain.Plutarch.Value         (passetClassValueOf, pvalTotalEntries)
import           Plutarch                                   (Term, pcon, plam, type (:-->), unTermCont, (#))
import           Plutarch.Api.V1                            (PPubKeyHash, PScriptPurpose (PSpending))
import qualified Plutarch.Api.V2                            as PV2
import           Plutarch.DataRepr                          (pfield)
import           Plutarch.Extra.RationalData
import           Plutarch.List                              (pfind)
import           Plutarch.Prelude                           (PAsData, PBool (..), PBuiltinList, PEq ((#==)), PInteger,
                                                             PMaybe (..), PPartialOrd ((#<=)), PUnit (..), getField,
                                                             pfromData, plength, pmatch)

mkPartialOrderConfigValidator :: forall s. Term s
                            (    PAssetClass
                            :--> PPartialOrderConfigDatum
                            :--> PUnit
                            :--> PV2.PScriptContext
                            :--> PUnit
                            )
mkPartialOrderConfigValidator
    = plam $ \nftAC d _ ctx -> unTermCont $ do

        ctxFs <- pletFieldsC @["txInfo", "purpose"] ctx
        info <- pletFieldsC
            @[ "inputs"
             , "outputs"
             , "signatories"
             , "datums"
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

        dFs <- pletFieldsC @[ "pocdSignatories"
                            , "pocdReqSignatories"
                            , "pocdNftSymbol"
                            ] d
        -- Assert multi-sig is correctly exercised.
        pguardC "missing signature(s)" $ phasSignatures # getField @"signatories" info # getField @"pocdSignatories" dFs # getField @"pocdReqSignatories" dFs

        -- Find continuing output with updated datum.
        outputs <- pletC $ getField @"outputs" info
        ownOutUtxo <- pletC $ pgetContinuingOutputUsingNft # getField @"address" ownInpUtxoFs # nftAC # outputs
        ownOutUtxoFs <- pletFieldsC @["value", "datum"] ownOutUtxo
        -- Continuing output does not have more than 10 tokens.
        pguardC "continuing output's value should have <= 10 tokens" $ pvalTotalEntries # getField @"value" ownOutUtxoFs #<= 10
        -- Assert new datum is of correct shape. Unlike PlutusTx, it checks whether credentials are of correct length, etc.
        newDatum <- pletC $ pfromData $ pparseDatum' @PPartialOrderConfigDatum # getField @"datum" ownOutUtxoFs # getField @"datums" info
        newDatumFs <- pletFieldsC @[ "pocdNftSymbol"
                                   , "pocdMakerFeeFlat"
                                   , "pocdMakerFeeRatio"
                                   , "pocdTakerFee"
                                   , "pocdMinDeposit"
                                   , "pocdReqSignatories"
                                   , "pocdSignatories"
                                   , "pocdFeeAddr"
                                   ] newDatum

        -- Check the fields of new datum and assert that they are bounded.

        -- @pocdSignatories@ are unique and their number lies b/w 1 & 10 (inclusive).
        -- Note that it is possible to dissolve multi-sig by giving a single signatory for which no corresponding key is known.
        newSigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC $ getField @"pocdSignatories" newDatumFs
        pguardC "duplicate signatories" $ pallUnique # newSigs
        -- We are iterating over list of signatories twice (earlier when determining duplicates and now, to determine length) but performance is not a concern here.
        newSigsNum <- pletC $ plength # newSigs
        pguardC "too many signatories" $ newSigsNum #<= 10
        pguardC "non-positive signatories" $ 1 #<= newSigsNum

        -- @pocdReqSignatories@ is positive and not more than the number of signatories.
        newReqSigs :: Term _ PInteger <- pletC $ getField @"pocdReqSignatories" newDatumFs
        pguardC "non-positive number of required signatories" $ 1 #<= newReqSigs
        pguardC "too many required signatories" $ newReqSigs #<= newSigsNum

        -- @pocdNftSymbol@ is not altered.
        pguardC "pocdNftSymbol changed" $ getField @"pocdNftSymbol" dFs #== getField @"pocdNftSymbol" newDatumFs

        -- Even though we have checked the format of fee address when parsing the datum, but to be sure of any edges, we assert that an output is made to this address as part of this transaction.
        newFeeAddr <- pletC $ getField @"pocdFeeAddr" newDatumFs
        pguardC "not paid to fee address" $
          -- We are iterating over list of outputs twice (traversed earlier when finding continuing output) but performance is not a concern here.
          pmatch (pfind # plam (\output -> pfield @"address" # output #== newFeeAddr) # outputs) $ \case
            PNothing -> pcon PFalse
            PJust _ -> pcon PTrue

        -- @pocdMakerFeeFlat@, @pocdTakerFee@ and @pocdMinDeposit@ are all non-negative and not more than 1000 ADA.
        let lovelaceThreshold = 1000_000_000
        newMakerFeeFlat :: Term _ PInteger <- pletC $ getField @"pocdMakerFeeFlat" newDatumFs
        pguardC "negative flat maker fee" $ 0 #<= newMakerFeeFlat
        pguardC "high flat maker fee" $ newMakerFeeFlat #<= lovelaceThreshold
        newTakerFee :: Term _ PInteger <- pletC $ getField @"pocdTakerFee" newDatumFs
        pguardC "negative taker fee" $ 0 #<= newTakerFee
        pguardC "high taker fee" $ newTakerFee #<= lovelaceThreshold
        newMinDeposit :: Term _ PInteger <- pletC $ getField @"pocdMinDeposit" newDatumFs
        pguardC "negative min ada deposit" $ 0 #<= newMinDeposit
        pguardC "high min ada deposit" $ newMinDeposit #<= lovelaceThreshold

        -- @pocdMakerFeeRatio@ is non-negative and not more than 1.
        newMakerFeeRatio :: Term _ PRationalData <- pletC $ getField @"pocdMakerFeeRatio" newDatumFs
        pguardC "negative maker fee ratio" $ 0 #<= (pfield @"numerator" # newMakerFeeRatio :: Term _ PInteger)
        -- Module @Plutarch.Extra.RationalData@ does not export constructor for @PRationalData@, so comparison is performed using `prationalFromData`.
        pguardC "high maker fee ratio" $ prationalFromData # newMakerFeeRatio #<= 1

        -- All good, we succeed.
        pure . pcon $ PUnit
