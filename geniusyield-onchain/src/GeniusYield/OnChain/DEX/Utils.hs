{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module GeniusYield.OnChain.DEX.Utils
    ( pfindMarkedRefInput
    , pfindFeeOutput
    , pcontinuingPartialOrder
    , pgetContainedFeeValue
    , pgetContainedFeeValue'
    , pcheckValidFillTime
    ) where

import           Plutarch.Api.V1                            (AmountGuarantees (..), KeyGuarantees (..), PAddress,
                                                             PCurrencySymbol, PDatum, PDatumHash, PMap (..),
                                                             PPOSIXTimeRange, PTokenName, PTxOutRef, PValue (..))
import qualified Plutarch.Api.V2                            as PV2
import           Plutarch.Extra.Map                         (pkvPairValue)
import           Plutarch.Extra.TermCont                    (pguardC, pguardC', pletC, pletFieldsC, pmatchC)
import           Plutarch.Prelude                           (PAsData, PBool (..), PBuiltinList, PBuiltinPair,
                                                             PEq ((#==)), PInteger, PListLike (..),
                                                             PMaybe (PJust, PNothing), PPair (..), Term, TermCont,
                                                             getField, pcon, pfield, pfind, pfoldl, pfromData,
                                                             pfstBuiltin, phoistAcyclic, pif, plam, plet, pletFields,
                                                             pmatch, precList, psndBuiltin, pto, ptraceError,
                                                             type (:-->), unTermCont, (#$), (#&&), (#))

import           GeniusYield.OnChain.DEX.PartialOrder.Types (PMaybePPOSIXTimeData (PPDJust, PPDNothing),
                                                             PPartialOrderConfigDatum, PPartialOrderContainedFee,
                                                             PPartialOrderFeeOutput (..))
import           GeniusYield.OnChain.Plutarch.Api           (pFrom, pTo, pcontains)
import           GeniusYield.OnChain.Plutarch.Tx            (presolveInlineDatum)
import           GeniusYield.OnChain.Plutarch.Types         (PAssetClass (..))
import           GeniusYield.OnChain.Plutarch.Utils         (pparseDatum')
import           GeniusYield.OnChain.Plutarch.Value         (passetClassValueOf, passetClassValuePositive, pgeq,
                                                             plovelace, psubtractValue)
import           Plutarch.Api.V1.AssocMap                   (plookup)
import           Plutarch.Api.V1.Value                      (pforgetPositive, pvalueOf)
import           Plutarch.Unsafe                            (punsafeCoerce)

pfindMarkedRefInput :: Term s (PBuiltinList PV2.PTxInInfo
                          :--> PAddress
                          :--> PAssetClass
                          :--> PPartialOrderConfigDatum)
pfindMarkedRefInput = phoistAcyclic $
    plam $ \inputs reqAddr reqAsset ->
        pmatch (pfind # (matches # reqAddr # reqAsset) # inputs) $ \case
            PNothing             -> ptraceError "reference input not found"
            PJust markedRefInput -> punsafeCoerce . pto $ presolveInlineDatum #$ pfield @"datum" #$ pfield @"resolved" # markedRefInput  -- Note that we are using `punsafeCoerce` here, so we don't verify whether datum is of correct type but since this UTxO is maintained by us, it would have correct structure.
  where
    matches :: Term s (PAddress :--> PAssetClass :--> PV2.PTxInInfo :--> PBool)
    matches = phoistAcyclic $
        plam $ \reqAddr reqAsset txininfo ->
            plet (pfield @"resolved" # txininfo) $
                \resolved ->
                        pletFields @'["address", "value"] resolved $ \resolved' ->
                        getField @"address" resolved' #== reqAddr
                    #&& passetClassValueOf # getField @"value" resolved' # reqAsset #== 1

pisFeeOutput :: Term s (PAddress :--> PV2.PTxOut :--> PBool)
pisFeeOutput = phoistAcyclic $
    plam $ \feeAddr txout ->
        pfield @"address" # txout #== feeAddr

pfindFeeOutput :: Term s (PBuiltinList PV2.PTxOut
                     :--> PMap 'Unsorted PDatumHash PDatum
                     :--> PAddress
                     :--> PAsData PInteger
                     :--> a
                     :--> (     PMap 'Unsorted PTxOutRef (PValue 'Sorted 'Positive)
                           :--> a)
                     :--> a)
pfindFeeOutput = phoistAcyclic $
    plam $ \outputs datums feeAddr takerFeeInt x f -> unTermCont $
        pmatchC (pfind # (pisFeeOutput # feeAddr) # outputs) >>= \case
            PNothing  -> pure x
            PJust out -> do

                outFs              <- pletFieldsC @'["value", "datum"] out
                outputValue        <- pletC $ getField @"value" outFs
                dat                <- pletC $ pfromData $ pparseDatum' @PPartialOrderFeeOutput # getField @"datum" outFs # datums  -- Note that it is important that we parse this datum as few constraints like amounts in `pofdReservedValue` must be positive.
                feeOutputHRec      <- pletFieldsC @'["pofdMentionedFees", "pofdReservedValue"] dat
                mentionedFees      <- pletC $ getField @"pofdMentionedFees" feeOutputHRec
                PMap kvs           <- pmatchC mentionedFees
                reservedValue      <- pletC $ getField @"pofdReservedValue" feeOutputHRec
                totalContainedFees <- pletC $ pfoldl
                    # plam (\acc kv ->
                        plet (pkvPairValue # kv) $ \containedFee ->
                        acc <> containedFee)
                    # mempty
                    # kvs
                actualFees    <- pletC $ psubtractValue # outputValue # reservedValue
                -- In following, even though we have done `pforgetPositive`, it doesn't change the fact that there would not be any @<= 0@ entry in @expectedFees@, this in turn would guarantee (by a later check) that there is no @<= 0@ entry in @actualFees@.
                let expectedFees = pforgetPositive (totalContainedFees <> (passetClassValuePositive # plovelace # takerFeeInt))
                pguardC "fee output's value is less than expected" $ pgeq # actualFees # expectedFees
                pure $ f # mentionedFees

-- | Given the currency symbol identifying partial orders, is the given output a continued partial order?
-- We need to check whether the output contains a token with the given symbol and that this token is not
-- being minted in the current transaction (in which case the output would be a freshly placed order).
-- If yes, return the corresponding token name; otherwise, return Nothing.
ppartialOrderToken :: Term s (PAddress :--> PCurrencySymbol :--> PValue 'Sorted 'NoGuarantees :--> PV2.PTxOut :--> PMaybe PTokenName)
ppartialOrderToken = phoistAcyclic $
    plam $ \ownAddr nftSymbol mint out -> unTermCont $ do
        out' <- pletFieldsC @["value", "address"] out
        pguardC' (pcon PNothing) (ownAddr #== getField @"address" out')
        PValue m <- pmatchC $ getField @"value" out'
        pmatchC (plookup # nftSymbol # m) >>= \case
            PNothing -> pure . pcon $ PNothing -- the output doesn't contain the right NFT
            PJust n  -> do
                PMap kvs <- pmatchC n
                pure $ pelimList (f nftSymbol mint) (pcon PNothing) kvs
  where
    f :: Term s PCurrencySymbol
      -> Term s (PValue 'Sorted 'NoGuarantees)
      -> Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
      -> Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
      -> Term s (PMaybe PTokenName)
    f nftSymbol mint kv _ =
        plet (pfromData $ pfstBuiltin # kv) $            \nftTkName ->
        pif (pfromData (psndBuiltin # kv) #== 1)  -- We require non-zero value, to safeguard if map is not normalized.
          (
          plet (pvalueOf # mint # nftSymbol # nftTkName) $ \minted    ->
          pif
              (minted #== 0)
              (pcon $ PJust nftTkName) -- the output is a continued partial order
              (pcon PNothing)          -- the output is a freshly placed order
          )
          (pcon PNothing)

-- | Given currency symbol and token name of the NFT identifying partial orders and a list of outputs,
--   tries to find an output containing that NFT.
--   If found, returns a boolean indicating whether the found output is the first partial order output in the list
--   and the output itself; otherwise, returns Nothing.
pcontinuingPartialOrder :: Term s ( PAddress
                               :--> PCurrencySymbol
                               :--> PTokenName
                               :--> PValue 'Sorted 'NoGuarantees
                               :--> PBuiltinList PV2.PTxOut
                               :--> PMaybe (PPair PBool PV2.PTxOut)
                                  )
pcontinuingPartialOrder = phoistAcyclic $
    plam $ \ownAddr nftSymbol nftTkName mint outputs ->
        go ownAddr nftSymbol nftTkName mint # outputs # pcon PTrue
  where
    go :: Term s PAddress
       -> Term s PCurrencySymbol
       -> Term s PTokenName
       -> Term s (PValue 'Sorted 'NoGuarantees)
       -> Term s (PBuiltinList PV2.PTxOut :--> PBool :--> PMaybe (PPair PBool PV2.PTxOut))
    go ownAddr nftSymbol nftTkName mint = precList
        (\self out xs -> plam $ \b ->
            pmatch (ppartialOrderToken # ownAddr # nftSymbol # mint # out) $ \case
                PNothing -> self # xs # b -- the output wasn't a continued partial order
                PJust tn -> pif
                    (tn #== nftTkName)
                    (pcon $ PJust $ pcon (PPair b out)) -- we found the order we were looking for
                    (self # xs # pcon PFalse)           -- we found a continued partial order,
                                                        -- but it was not the one we were looking for
        )
        (\_self -> plam $ \_b -> pcon PNothing)

pgetContainedFeeValue ::
  Term s ( PPartialOrderContainedFee
      :--> PAssetClass
      :--> PAssetClass
      :--> PValue 'Sorted 'Positive
         )
pgetContainedFeeValue = phoistAcyclic $ plam $ \containedFee offAC askAC
  -> unTermCont $ do
    containedFeeHRec <- pletFieldsC @[
                                   "pocfLovelaces"
                                 , "pocfOfferedTokens"
                                 , "pocfAskedTokens"
                                 ] containedFee

    containedFeeLov  <- pletC $ getField @"pocfLovelaces" containedFeeHRec
    containedFeeOff  <- pletC $ getField @"pocfOfferedTokens" containedFeeHRec
    containedFeeAsk  <- pletC $ getField @"pocfAskedTokens" containedFeeHRec
    pure $ pgetContainedFeeValue' # offAC # askAC # containedFeeLov # containedFeeOff # containedFeeAsk

pgetContainedFeeValue' ::
  Term s ( PAssetClass
      :--> PAssetClass
      :--> PAsData PInteger
      :--> PAsData PInteger
      :--> PAsData PInteger
      :--> PValue 'Sorted 'Positive
         )
pgetContainedFeeValue' = phoistAcyclic $ plam $ \offAC askAC lov off ask
  -> passetClassValuePositive # plovelace # lov <> passetClassValuePositive # offAC # off <> passetClassValuePositive # askAC # ask

pcheckValidStartTime :: Term s (PPOSIXTimeRange :--> PMaybePPOSIXTimeData :--> PBool)
pcheckValidStartTime = phoistAcyclic $ plam $ \validRange mstart' ->
  pmatch mstart' $ \case
    PPDNothing _ -> pcon PTrue
    PPDJust rec  -> pcontains # (pFrom # (pfield @"_0" # rec)) # validRange

pcheckValidEndTime :: Term s (PPOSIXTimeRange :--> PMaybePPOSIXTimeData :--> PBool)
pcheckValidEndTime = phoistAcyclic $ plam $ \validRange mend' ->
  pmatch mend' $ \case
    PPDNothing _ -> pcon PTrue
    PPDJust rec  -> pcontains # (pTo # (pfield @"_0" # rec)) # validRange

pcheckValidFillTime :: Term s PPOSIXTimeRange
                    -> Term s PMaybePPOSIXTimeData
                    -> Term s PMaybePPOSIXTimeData
                    -> TermCont s ()
pcheckValidFillTime validRange' mstart' mend' = do
  validRange <- pletC validRange'
  pguardC "filled too early" $ pcheckValidStartTime # validRange # mstart'
  pguardC "filled too late" $ pcheckValidEndTime # validRange # mend'
