{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -O2 -fspecialize-aggressively #-}

module GeniusYield.OnChain.DEX.PartialOrderNFT (mkPartialOrderNFTPolicy) where

import           Prelude                                    (Bool (..), Semigroup (..), pure, ($), (.))

import           Plutarch.Api.V1                            (AmountGuarantees (..), KeyGuarantees (Sorted, Unsorted),
                                                             PAddress, PCurrencySymbol, PDatum, PDatumHash, PMap,
                                                             PMaybeData (..), PTokenName, PTxOutRef, PValue)
import qualified Plutarch.Api.V1.AssocMap                   as PMap
import           Plutarch.Api.V1.Scripts                    (PScriptHash)
import           Plutarch.Api.V1.Value                      (pforgetPositive)
import qualified Plutarch.Api.V2                            as PV2
import           Plutarch.Extra.RationalData                (PRationalData, prationalFromData)
import           Plutarch.Num                               (PNum ((#*)))
import           Plutarch.Prelude                           (PAsData, PBuiltinList, PEq ((#==)), PInteger,
                                                             PListLike (pelimList, pnull), PMaybe (PJust, PNothing),
                                                             PPartialOrd ((#<), (#<=)), PUnit, Term, getField, pany,
                                                             pconstant, pdata, perror, pfield, pfromData, pfstBuiltin,
                                                             pif, plam, plet, pletFields, pmatch, pnot, psndBuiltin,
                                                             pto, ptraceError, type (:-->), unTermCont, (#$), (#&&),
                                                             (#))
import qualified Plutarch.Rational                          as Rational

import           GeniusYield.OnChain.DEX.PartialOrder.Types (PMaybePPOSIXTimeData (..), PPartialOrderContainedFee (..),
                                                             PPartialOrderDatum (..))
import           GeniusYield.OnChain.DEX.Utils              (pfindMarkedRefInput, pgetContainedFeeValue')
import           GeniusYield.OnChain.Plutarch.Api           (PAssetClass, pexpectedTokenName, pguardC, pletC,
                                                             pletFieldsC, pmatchC, pownSymbol, putxoConsumed)
import           GeniusYield.OnChain.Plutarch.Utils         (pceiling, pfindOutputWithNft, pisAddressForScript,
                                                             pparseDatum')
import           GeniusYield.OnChain.Plutarch.Value         (passetClass, passetClassValue, pgeq, plovelace)

mkPartialOrderNFTPolicy ::
  Term s (PScriptHash
     :--> PAddress
     :--> PAssetClass
     :--> PAsData (PMaybeData PTxOutRef)
     :--> PV2.PScriptContext
     :--> PUnit
         )
mkPartialOrderNFTPolicy = plam $ \sh refInputAddr refInputToken mtxOutRef ctx ->
    policy sh refInputAddr refInputToken (pfromData mtxOutRef) ctx
  where
    policy ::
          Term s PScriptHash
      ->  Term s PAddress
      ->  Term s PAssetClass
      ->  Term s (PMaybeData PTxOutRef)
      ->  Term s PV2.PScriptContext
      ->  Term s PUnit

    policy sh refInputAddr refInputToken txOutRef ctx =
        plet (pownSymbol # ctx) $ \ownSymbol ->
        plet (pfield @"txInfo" # ctx) $ \info ->
        pletFields @'[ "inputs"
                     , "referenceInputs"
                     , "outputs"
                     , "datums"
                     , "mint"
                     ] info $ \infoFs ->
        plet (getField @"mint" infoFs) $ \mint ->
        pmatch txOutRef $ \case
            PDNothing _ -> checkIfBurning # ownSymbol # mint

            PDJust rec  -> plet (pfield @"_0" # rec) $ \ref ->
                           plet (pfindMarkedRefInput # getField @"referenceInputs" infoFs # refInputAddr # refInputToken) $ \markedRefDatum ->
                           pletFields @'[ "pocdMakerFeeFlat"
                                        , "pocdMakerFeeRatio"
                                        , "pocdTakerFee"
                                        , "pocdMinDeposit"
                                        ] markedRefDatum $ \markedRefDatumFs ->
                           validateMinting                                    #
                               sh                                             #
                               ref                                            #
                               ownSymbol                                      #
                               (pexpectedTokenName # ref)                     #
                               getField @"inputs" infoFs                      #
                               getField @"outputs" infoFs                     #
                               getField @"datums" infoFs                      #
                               getField @"pocdMakerFeeFlat" markedRefDatumFs  #
                               getField @"pocdMakerFeeRatio" markedRefDatumFs #
                               getField @"pocdTakerFee" markedRefDatumFs      #
                               getField @"pocdMinDeposit" markedRefDatumFs    #
                               (mintedTokens_ # ownSymbol # mint)

    validateMinting ::
        Term s (PScriptHash
          :-->  PTxOutRef
          :-->  PCurrencySymbol
          :-->  PTokenName
          :-->  PBuiltinList PV2.PTxInInfo
          :-->  PBuiltinList PV2.PTxOut
          :-->  PMap 'Unsorted PDatumHash PDatum
          :-->  PInteger
          :-->  PRationalData
          :-->  PInteger
          :-->  PInteger
          :-->  PMap any PTokenName PInteger
          :-->  PUnit
               )
    validateMinting = plam $
        \sh txOutRef cs tn inputs outputs datums makerFeeFlatContract makerFeeRatioContract takerFeeFlatContract deposit ->
          let
            hasUtxoConsumed  = putxoConsumed # txOutRef # inputs
            mintedTnAmt   x  = pfromData (psndBuiltin # x)
            mintedTnName  x  = pfromData (pfstBuiltin # x)
            errMsg           = "Expected: 1.) UTxO consumption. 2.) Minted Amount should be 1. 3.) Valid Token name. 4.) Correct datum."
          in
            pelimList (\h t -> pif
                (  hasUtxoConsumed
                #&& pnull # t
                #&& mintedTnAmt h  #== 1
                #&& mintedTnName h #== tn
                )
                (checkOutput # sh # deposit # outputs # datums # cs # tn # makerFeeFlatContract # makerFeeRatioContract # takerFeeFlatContract)
                (ptraceError errMsg)
                ) (ptraceError "minted tokens list should not be empty.") . pto

    checkIfBurning ::
        Term s (PCurrencySymbol
          :-->  PValue 'Sorted 'NoGuarantees
          :-->  PUnit
               )
    checkIfBurning = plam $
        \cs mint -> pif
            (pany #
            plam (\v -> 0 #<= pfromData (psndBuiltin # v)) #
            pto (mintedTokens_ # cs # mint)
            )
            (ptraceError "expected only burning")
            (pconstant ())

    checkOutput :: Term s (PScriptHash
                      :--> PInteger
                      :--> PBuiltinList PV2.PTxOut
                      :--> PMap 'Unsorted PDatumHash PDatum
                      :--> PCurrencySymbol
                      :--> PTokenName
                      :--> PInteger
                      :--> PRationalData
                      :--> PInteger
                      :--> PUnit
                          )
    checkOutput = plam $
        \sh deposit outputs datums cs tn makerFeeFlatContract makerFeeRatioContract takerFeeFlatContract -> unTermCont $ do
            nftAC       <- pletC $ passetClass # cs # tn
            PJust txOut <- pmatchC $ pfindOutputWithNft # outputs # nftAC

            pguardC "expected NFT to go to the script" $
                pisAddressForScript # (pfield @"address" # txOut) # sh

            od   <- pletC $ pparseDatum' @PPartialOrderDatum # (pfield @"datum" # txOut) # datums
            odFs <- pletFieldsC @[
                                   "podOfferedAsset"
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

            (offeredAmount         :: Term _ PInteger)                   <- pletC $ getField @"podOfferedAmount"          odFs
            (offeredOriginalAmount :: Term _ PInteger)                   <- pletC $ getField @"podOfferedOriginalAmount"  odFs
            (partialFills          :: Term _ PInteger)                   <- pletC $ getField @"podPartialFills"           odFs
            (askedPrice            :: Term _ PRationalData)              <- pletC $ getField @"podPrice"                  odFs
            (containedFee          :: Term _ PPartialOrderContainedFee)  <- pletC $ getField @"podContainedFee"           odFs
            (containedPayment      :: Term _ PInteger)                   <- pletC $ getField @"podContainedPayment"       odFs
            (askedPriceN           :: Term _ PInteger)                   <- pletC $ pfield @"numerator" # askedPrice
            makerFeeFlatDat                                              <- pletC $ getField @"podMakerLovelaceFlatFee"   odFs
            takerFeeFlatDat                                              <- pletC $ getField @"podTakerLovelaceFlatFee" odFs

            containedFeeHRec <- pletFieldsC @[
                                   "pocfLovelaces"
                                 , "pocfOfferedTokens"
                                 , "pocfAskedTokens"
                                 ] containedFee
            containedFeeLov  <- pletC $ getField @"pocfLovelaces" containedFeeHRec
            containedFeeOff  <- pletC $ getField @"pocfOfferedTokens" containedFeeHRec
            containedFeeAsk  <- pletC $ getField @"pocfAskedTokens" containedFeeHRec
            offeredAsset     <- pletC $ getField @"podOfferedAsset" odFs
            askedAsset       <- pletC $ getField @"podAskedAsset" odFs
            orderStart       <- pmatchC $ getField @"podStart" odFs
            orderEnd         <- pmatchC $ getField @"podEnd" odFs

            pguardC "offered amount should be positive" $
                0 #< offeredAmount
            pguardC "offered amount should equal original amount" $
                offeredOriginalAmount #== offeredAmount
            pguardC "wrong name for NFT" $
                getField @"podNFT" odFs #== tn
            pguardC "number of partial fills must be zero" $
                0 #== partialFills
            pguardC "podPrice must be positive" $
                0 #< askedPriceN
            pguardC "podOfferedAsset must be different than podAskedAsset" $
                pnot #$ offeredAsset #== askedAsset
            pguardC "order end shouldn't be earlier than start" $
                case (orderStart, orderEnd) of
                  (PPDJust orderStart', PPDJust orderEnd') -> orderStart' #<= orderEnd'
                  _otherwise                               -> pconstant True
            pguardC "podMakerLovelaceFlatFee is not same as that mentioned pocfLovelaces" $
                makerFeeFlatDat #== containedFeeLov
            pguardC "podTakerLovelaceFlatFee is not the same as mentioned in config contract" $
                takerFeeFlatDat #== takerFeeFlatContract

            offeredAssets <- pletC $ passetClassValue # offeredAsset # pdata offeredAmount
            nft           <- pletC $ passetClassValue # nftAC # pdata 1
            depositValue  <- pletC $ passetClassValue # plovelace # pdata deposit
            containedFee' <- pletC $ pforgetPositive $ pgetContainedFeeValue' # offeredAsset # askedAsset # containedFeeLov # containedFeeOff # containedFeeAsk
            expectedOffer <- pletC $ offeredAssets <> nft <> depositValue <> containedFee'
            actualOffer   <- pletC $ pforgetPositive $ pfromData (pfield @"value" # txOut)

            pguardC "offer too low" $
                pgeq # actualOffer # expectedOffer


            pguardC "pocfAskedTokens in podContainedFee should be zero" $
                pfromData containedFeeAsk #== 0

            pguardC "fees too low" $
                    makerFeeFlatContract #<= pfromData containedFeeLov
                #&& pceiling # ((prationalFromData # makerFeeRatioContract) #* (Rational.pfromInteger # offeredAmount)) #<= pfromData containedFeeOff

            pguardC "contained payment must be zero" $
                containedPayment #== 0

            pure . pconstant $ ()

mintedTokens_ ::
  Term s (PCurrencySymbol
     :--> PValue 'Sorted 'NoGuarantees
     :--> PMap 'Sorted PTokenName PInteger
         )
mintedTokens_ = plam $
    \cs mint ->
      pmatch (PMap.plookup # cs # pto mint) $
        \case
          PNothing -> perror
          PJust m  -> m
