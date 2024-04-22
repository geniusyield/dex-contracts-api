{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GeniusYield.OnChain.DEX.PartialOrder (mkPartialOrderValidator) where

import           Plutarch                                   (Term, pcon, plam, type (:-->), unTermCont, (#$), (#))
import           Plutarch.Api.V1                            (PAddress, PScriptPurpose (PSpending))
import qualified Plutarch.Api.V2                            as PV2
import           Plutarch.DataRepr                          (pdcons, pdnil, pfield)
import           Plutarch.Extra.RationalData                (prationalFromData)
import           Plutarch.Prelude                           (PBool (..), PEq ((#==)), PInteger, PMaybe (..), PPair (..),
                                                             PPartialOrd ((#<), (#<=)), PUnit (..), getField, pdata,
                                                             pfromData, pif, pmatch, pnot, ptraceError, (#&&), (#||))
import qualified Plutarch.Rational                          as PRational

import           GeniusYield.OnChain.DEX.PartialOrder.Types (PPartialOrderAction (..),
                                                             PPartialOrderContainedFee (PPartialOrderContainedFee),
                                                             PPartialOrderDatum (..))
import           Plutarch.Api.V1.AssocMap                   (plookup)
import           Plutarch.Api.V1.Value                      (AmountGuarantees (..), KeyGuarantees (..), PValue,
                                                             pforgetPositive, pvalueOf)

import           GeniusYield.OnChain.DEX.Utils              (pcheckValidFillTime, pcontinuingPartialOrder,
                                                             pfindFeeOutput, pfindMarkedRefInput, pgetContainedFeeValue,
                                                             pgetContainedFeeValue')
import           GeniusYield.OnChain.Plutarch.Api           (PAssetClass, pguardC, pguardC', pletC, pletFieldsC,
                                                             pmatchC, ppaidValuePlusInline, presolveDatum, ptxSignedBy,
                                                             toDatum)
import           GeniusYield.OnChain.Plutarch.Utils         (pceiling, pfindOwnInput, pfloor)
import           GeniusYield.OnChain.Plutarch.Value         (passetClass, passetClassValueOf, passetClassValuePositive,
                                                             pgeq, plovelace, psubtractValue, pvalTotalEntries)
import           Plutarch.Unsafe                            (punsafeCoerce)

mkPartialOrderValidator :: forall s. Term s
                          (    PAddress
                          :--> PAssetClass
                          :--> PPartialOrderDatum
                          :--> PPartialOrderAction
                          :--> PV2.PScriptContext
                          :--> PUnit
                          )
mkPartialOrderValidator
    = plam $ \refInputAddr refInputToken od oa ctx -> unTermCont $ do
        odFs <- pletFieldsC @[ "podOwnerKey"
                             , "podOwnerAddr"
                             , "podOfferedAsset"
                             , "podOfferedOriginalAmount"
                             , "podOfferedAmount"
                             , "podAskedAsset"
                             , "podPrice"
                             , "podNFT"
                             , "podStart"
                             , "podEnd"
                             , "podPartialFills"
                             , "podMakerLovelaceFlatFee"
                             , "podTakerLovelaceFlatFee"
                             , "podContainedFee"
                             , "podContainedPayment"
                             ] od

        ctxFs <- pletFieldsC @["txInfo", "purpose"] ctx

        PSpending spRec <- pmatchC $ getField @"purpose" ctxFs
        ownRef <- pletC $ pfield @"_0" # spRec

        info <- pletFieldsC
            @[ "inputs"
             , "referenceInputs"
             , "outputs"
             , "mint"
             , "validRange"
             , "signatories"
             , "datums"
             ]
             $ getField @"txInfo" ctxFs

        PJust ownInput   <- pmatchC $ pfindOwnInput # getField @"inputs" info # ownRef
        nftTkName        <- pletC $ getField @"podNFT" odFs
        ownInpUtxo       <- pletFieldsC @["value", "address"] $ pfield @"resolved" # ownInput
        valueIn :: Term _ (PValue 'Sorted 'Positive)
                         <- pletC $ getField @"value" ownInpUtxo
        markedRefDatum   <- pletC $ pfindMarkedRefInput # getField @"referenceInputs" info # refInputAddr # refInputToken
        markedRefDatumFs <- pletFieldsC
            @'[ "pocdNftSymbol"
              , "pocdFeeAddr"
              ] markedRefDatum
        nftSymbol        <- pletC $ getField @"pocdNftSymbol" markedRefDatumFs
        feeAddr          <- pletC $ getField @"pocdFeeAddr" markedRefDatumFs

        -- Succeed immediately if there is no NFT.
        pguardC' (pcon PUnit) $ pvalueOf # valueIn # nftSymbol # nftTkName #== 1

        datums          <- pletC $ getField @"datums" info
        outputs         <- pletC $ getField @"outputs" info
        mint            <- pletC $ getField @"mint" info
        nftMintAmt      <- pletC $ pvalueOf # mint # nftSymbol # nftTkName
        offeredAmount   <- pletC $ getField @"podOfferedAmount" odFs
        offAC           <- pletC $ getField @"podOfferedAsset" odFs
        askAC           <- pletC $ getField @"podAskedAsset" odFs
        podPrice        <- pletC $ getField @"podPrice" odFs
        ownerAddr       <- pletC $ getField @"podOwnerAddr" odFs
        curContainedFee :: Term _ PPartialOrderContainedFee
                        <- pletC $ getField @"podContainedFee" odFs
        (priceInteger :: Term s (PInteger :--> PInteger))
                        <- pletC $ plam $
            \amt ->
              pceiling #$
                (PRational.pfromInteger # amt) * (prationalFromData # podPrice)

        {- | Note that at any moment, an order UTxO contains:-
               * An NFT.
               * Remaining offered tokens.
               * Payment for sold tokens.
               * Initial deposit.
               * Collected fees.
        -}
        curPaymentWithDeposit :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'NoGuarantees)
                        <- pletC $ plam $
            \contFeeVal ->
              let toSubtract = (passetClassValuePositive # (passetClass # nftSymbol # nftTkName) # pdata 1) <> (passetClassValuePositive # offAC # pdata offeredAmount) <> contFeeVal
                  -- In above, @contFeeVal@ is simply @pgetContainedFeeValue # curContainedFee # offAC # askAC@ but in case @curContainedFee@ is already destructured, it might be more efficient to construct value using destructured components and send it instead.
                  -- For complete fill case, one should also add payment for consumed tokens to know for outgoing payment.
              in psubtractValue # valueIn # toSubtract

        pmatchC oa >>= \case

            PPartialCancel _ ->  do
                pguardC "not signed by owner" $ ptxSignedBy
                                                    # getField @"podOwnerKey" odFs
                                                    # getField @"signatories" info
                pguardC "NFT not burnt" $ nftMintAmt #== -1

                curContFeeHRec <- pletFieldsC @["pocfLovelaces", "pocfOfferedTokens", "pocfAskedTokens"] curContainedFee
                feeLov         <- pletC (getField @"pocfLovelaces" curContFeeHRec)
                feeOff         <- pletC (getField @"pocfOfferedTokens" curContFeeHRec)
                feeAsk         <- pletC (getField @"pocfAskedTokens" curContFeeHRec)

                -- Checking if payment is made specifically to `podOwnerAddr`.
                let expectedPayment  = curPaymentWithDeposit # (pgetContainedFeeValue' # offAC # askAC # feeLov # feeOff # feeAsk)
                    actualPayment    = ppaidValuePlusInline # ownRef # ownerAddr # datums # outputs

                pguardC "insufficient payment" $
                    pgeq # pforgetPositive actualPayment # expectedPayment

                (partialFills :: Term _ PInteger) <- pletC $ getField @"podPartialFills" odFs
                pmatchC (partialFills #== 0 #|| curContainedFee #== mempty) >>= \case  -- Note that later we subtract from this @curContainedFee@ to know for the required contained fee, but if required contained fee is @mempty@ then it can be mathematically shown that @curContainedFee@ is also @mempty@.

                    PTrue  -> pure . pcon $ PUnit  -- no partial fills or no fees, maker fees go back to the maker

                    PFalse -> do  -- fees need to be paid

                      pguardC "fee not paid" $
                        pfindFeeOutput
                          #  outputs
                          #  datums
                          #  feeAddr
                          #  pdata 0
                          #  pcon PFalse
                          #$ plam $ \m ->
                              pmatch (plookup # ownRef # m) $ \case
                                PNothing           -> pcon PFalse
                                PJust mentionedFee ->

                                  let originalOfferedAmount :: Term _ PInteger = getField @"podOfferedOriginalAmount" odFs
                                      feeToRefund = pfloor #$ (PRational.pfromInteger # offeredAmount) * pcon (PRational.PRational (pfromData feeOff) (punsafeCoerce originalOfferedAmount))  -- We are using `punsafeCoerce` instead of `ptryPositive` as our NFT policy checks that original offered amount is positive.
                                  in ((pgetContainedFeeValue' # offAC # askAC # feeLov # pdata (pfromData feeOff - feeToRefund) # feeAsk) #<= mentionedFee)

                      pure . pcon $ PUnit

            PCompleteFill _ -> do

                pguardC "NFT not burnt" $ nftMintAmt #== -1

                pcheckValidFillTime (getField @"validRange" info) (getField @"podStart" odFs) (getField @"podEnd" odFs)

                -- We must check that the payment to the maker is correct.
                -- It is the sum of the payment from previous partial fills contained in the order
                -- and the price of the remaining offered amount.
                curContainedFeeValue <- pletC $ pgetContainedFeeValue # curContainedFee # offAC # askAC
                let expectedPayment  = (curPaymentWithDeposit # curContainedFeeValue) <> pforgetPositive (passetClassValuePositive # askAC # pdata (priceInteger # offeredAmount))
                    actualPayment    = ppaidValuePlusInline # ownRef # ownerAddr # datums # outputs
                pguardC "insufficient payment" $
                    pgeq # pforgetPositive actualPayment # expectedPayment

                -- We must check that the fees are paid.
                -- They consist of the contained fees and the taker fee.
                takerFeeInt        <- pletC $ getField @"podTakerLovelaceFlatFee" odFs
                isContainedFeeZero <- pletC $ curContainedFee #== mempty
                isTotalFeeZero     <- pletC $ isContainedFeeZero #&& takerFeeInt #== pdata 0
                pguardC "fee not paid" $
                    pfindFeeOutput
                        #  outputs
                        #  datums
                        #  feeAddr
                        #  takerFeeInt
                        #  isTotalFeeZero  -- if fees are zero, we don't need a fee output
                        #$ plam $ \m ->
                            pmatch (plookup # ownRef # m) $ \case
                                PNothing            -> isContainedFeeZero  -- if contained fees are zero, it's fine not to have an entry in the map
                                PJust mentionedFee  ->
                                    curContainedFeeValue #<= mentionedFee  -- contained fees paid

                pure . pcon $ PUnit

            PPartialFill pf -> do
                amt                 <- pletC $ pfield @"_0" # pf

                pguardC "amount must be positive"
                  (0 #< amt)
                pguardC "amount must be less than offered amount"
                  (amt #< offeredAmount)

                pcheckValidFillTime (getField @"validRange" info) (getField @"podStart" odFs) (getField @"podEnd" odFs)

                oldContainedFeeHRec <- pletFieldsC @[
                                              "pocfLovelaces"
                                            , "pocfOfferedTokens"
                                            , "pocfAskedTokens"
                                            ] curContainedFee
                oldContainedFeeLov  <- pletC $ getField @"pocfLovelaces" oldContainedFeeHRec
                oldContainedFeeOff  <- pletC $ getField @"pocfOfferedTokens" oldContainedFeeHRec
                oldContainedFeeAsk  <- pletC $ getField @"pocfAskedTokens" oldContainedFeeHRec
                oldContainedPayment <- pletC $ getField @"podContainedPayment" odFs
                priceInteger'       <- pletC $ priceInteger # amt
                newContainedPayment <- pletC $ oldContainedPayment + priceInteger'
                takerFeeInt         <- pletC $ getField @"podTakerLovelaceFlatFee" odFs
                takerFee            <- pletC $ passetClassValuePositive # plovelace # takerFeeInt

                -- check whether there is a fee output (which must then contain the taker fee)
                hasFeeOutput        <- pletC $ pfindFeeOutput
                    #  outputs
                    #  datums
                    #  feeAddr
                    #  takerFeeInt
                    #  pcon PFalse  -- no fee output found; taker fees must be in the first continued partial order output
                    #$ plam $ \_ -> pcon PTrue  -- fee output containing taker fees found

                -- search for "our" continuing output
                pmatchC (pcontinuingPartialOrder # getField @"address" ownInpUtxo # nftSymbol # nftTkName # mint # outputs) >>= \case
                    PNothing -> pure $ ptraceError "continuing output not found"
                    PJust p  -> do
                        PPair isFirst out                          <- pmatchC p
                        PPair expectedValueOut newContainedFeeLov  <- pmatchC $ unTermCont $ do
                            price <- pletC $ passetClassValuePositive # askAC # pdata priceInteger'
                            sold  <- pletC $ passetClassValuePositive # offAC # pdata amt
                            v     <- pletC $ pforgetPositive price <> (psubtractValue # pforgetPositive valueIn # sold)
                            pure $ pif (isFirst #&& pnot # hasFeeOutput)

                                -- taker fee must be included in our continuing output
                                (pcon $ PPair (v <> pforgetPositive takerFee) (pdata $ pfromData oldContainedFeeLov + pfromData takerFeeInt))

                                -- taker fee is either included in a fee output or in another continuing output;
                                -- in the latter case, we don't need to check anything, because the validator of that
                                -- order will do the check
                                (pcon $ PPair v oldContainedFeeLov)

                        outFs                   <- pletFieldsC @'["value", "datum"] out
                        actualDatum             <- pletC $ presolveDatum # getField @"datum" outFs # datums
                        actualValueOut :: Term _ (PValue 'Sorted 'Positive)
                                                <- pletC $ getField @"value" outFs
                        diffActualExpectedValue <- pletC $ psubtractValue # pforgetPositive actualValueOut # expectedValueOut  -- Note that all amounts in @diffActualExpectedValue@ are @>= 0@ by courtesy of a check done shortly later.
                        additionalAskTokens     <- pletC $ passetClassValueOf # diffActualExpectedValue # askAC
                        additionalLovelaces     <- pletC $ pif (askAC #== plovelace) 0 (passetClassValueOf # diffActualExpectedValue # plovelace)
                        let newContainedFeeDat =
                                    pcon $ PPartialOrderContainedFee
                                        $  pdcons @"pocfLovelaces" # pdata (pfromData newContainedFeeLov + additionalLovelaces)
                                        #$ pdcons @"pocfOfferedTokens" # oldContainedFeeOff
                                        #$ pdcons @"pocfAskedTokens" # pdata (oldContainedFeeAsk + additionalAskTokens)
                                        #$ pdnil
                        let expectedDatum =
                                    pcon $ PPartialOrderDatum
                                        $  pdcons @"podOwnerKey" # getField @"podOwnerKey" odFs
                                        #$ pdcons @"podOwnerAddr" # pdata ownerAddr
                                        #$ pdcons @"podOfferedAsset" # pdata offAC
                                        #$ pdcons @"podOfferedOriginalAmount" # getField @"podOfferedOriginalAmount" odFs
                                        #$ pdcons @"podOfferedAmount" # pdata (offeredAmount - amt)
                                        #$ pdcons @"podAskedAsset" # pdata askAC
                                        #$ pdcons @"podPrice" # pdata podPrice
                                        #$ pdcons @"podNFT" # pdata nftTkName
                                        #$ pdcons @"podStart" # getField @"podStart" odFs
                                        #$ pdcons @"podEnd" # getField @"podEnd" odFs
                                        #$ pdcons @"podPartialFills" # pdata (getField @"podPartialFills" odFs + 1)
                                        #$ pdcons @"podMakerLovelaceFlatFee" # getField @"podMakerLovelaceFlatFee" odFs
                                        #$ pdcons @"podTakerLovelaceFlatFee" # takerFeeInt
                                        #$ pdcons @"podContainedFee" # pdata newContainedFeeDat
                                        #$ pdcons @"podContainedPayment" # pdata newContainedPayment
                                        #$ pdnil

                        pguardC "insufficient value in continuing output"
                          (pgeq # pforgetPositive actualValueOut # expectedValueOut)
                        pguardC "wrong datum in continuing output"
                          (actualDatum #== toDatum expectedDatum)
                        pguardC "continuing output's value should have <= 10 tokens"  -- To prevent against token dust attack as order author should always be able to cancel their order. Note that filler cannot increase datum size arbitrarily.
                          (pvalTotalEntries # actualValueOut #<= 10)

                        pure . pcon $ PUnit
