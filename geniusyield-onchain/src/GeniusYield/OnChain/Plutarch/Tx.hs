{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module GeniusYield.OnChain.Plutarch.Tx  ( pownUtxo
                                        , putxoConsumed
                                        , ptxSignedBy
                                        , phasSignatures
                                        , pfindTxOutByTxOutRef
                                        , toDatum
                                        , pownSymbol
                                        , pexpectedTokenName
                                        , ppaidValue
                                        , ppaidValuePlusInline
                                        , ppaidValueSum
                                        , pmintedTokens
                                        , pdecodeInlineDatum
                                        , presolveInlineDatum
                                        , presolveDatum
                                        ) where

import           Plutarch.Api.V1
import qualified Plutarch.Api.V1.AssocMap           as PMap
import           Plutarch.Api.V2                    (POutputDatum (POutputDatumHash))
import qualified Plutarch.Api.V2                    as PV2
import           Plutarch.Builtin                   (pforgetData)
import           Plutarch.Extra.TermCont            (pletC, pletFieldsC, pmatchC)
import           Plutarch.Prelude

import           GeniusYield.OnChain.Plutarch.Utils (pelem', pfromMaybe, ptryFromData)

-- $setup
--
-- >>> :set -XOverloadedStrings -XDataKinds
-- >>> import           GeniusYield.OnChain.Plutarch.Run
-- >>> import           Plutarch.Prelude
-- >>> import           Plutarch
-- >>> import           Plutarch.Api.V1
-- >>> import           Plutarch.Extra.Maybe               (pjust, pnothing)
-- >>> import qualified PlutusTx
-- >>> import qualified PlutusLedgerApi.V1.Crypto as PlutusTx
--
-- >>> let pub1 = pdata (pconstant "325b145b838462dcacef5352c2fc99d4b3cc7455de15851cd88d2903")
-- >>> let pub2 = pdata (pconstant "325b145b838462dcacef5352c2fc99d4b3cc7455de15851cd88d2904")
-- >>> let pub3 = pdata (pconstant "325b145b838462dcacef5352c2fc99d4b3cc7455de15851cd88d2905")
-- >>> let pub4 = pdata (pconstant "325b145b838462dcacef5352c2fc99d4b3cc7455de15851cd88d2906")

-- | 'pownUtxo' returns the 'PTxOutRef' that tx is trying to spend, fails if 'PScriptContext'
--   doesn't have 'PSpending'.
pownUtxo ::
  Term s (PV2.PScriptContext
     :--> PTxOutRef
         )
pownUtxo = phoistAcyclic $ plam $ \ctx
  -> unTermCont $ do
     let purpose = pfromData $ pfield @"purpose" # ctx
     PSpending x <- pmatchC purpose
     return (pfield @"_0" # x)


-- | 'putxoConsumed' checks if a given utxo is consumed.
putxoConsumed ::
  Term s (PTxOutRef
     :--> PBuiltinList PV2.PTxInInfo
     :--> PBool
         )
putxoConsumed = phoistAcyclic $ plam $ \txOutRef inputs
  -> pany #
     plam (\x -> txOutRef #== pfield @"outRef" # x) # inputs

-- | 'ptxSignedBy' checks if a tx is signed by particular 'PPubKeyHash'.
ptxSignedBy ::
  Term s (PPubKeyHash
     :--> PBuiltinList (PAsData PPubKeyHash)
     :--> PBool
         )
ptxSignedBy = phoistAcyclic $ plam $ \sig sigs
  -> pelem # pdata sig # sigs

-- | Determines if intersection of transaction signatories and authorized signatores satisfy the quorum.
--
-- >>> evalT $ phasSignatures # pnil # pnil # 0
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 1760851, exBudgetMemory = ExMemory 6133},[])
--
-- >>> evalT $ phasSignatures # (pcons # pub1 # pnil) # (pcons # pub1 # pnil) # 1
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 6953448, exBudgetMemory = ExMemory 18659},[])
--
-- >>> evalT $ phasSignatures # (pcons # pub1 #$ pcons # pub1 # pnil) # (pcons # pub1 #$ pcons # pub2 # pnil) # 2
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool False))}},ExBudget {exBudgetCPU = ExCPU 12275922, exBudgetMemory = ExMemory 31915},[])
--
-- >>> evalT $ phasSignatures # (pcons # pub4 #$ pcons # pub3 #$ pcons # pub2 # pnil) # (pcons # pub1 #$ pcons # pub2 #$ pcons # pub3 # pnil) # 2
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 30821725, exBudgetMemory = ExMemory 69123},[])
--
phasSignatures ::
  Term s (PBuiltinList (PAsData PPubKeyHash)
          -- ^ Transaction signatories.
     :--> PBuiltinList (PAsData PPubKeyHash)
          -- ^ Authorized signatories.
     :--> PInteger
          -- ^ Quorum.
     :--> PBool
         )
phasSignatures =
  phoistAcyclic $
    plam $ \txSignatories authorizedSignatores reqSigs ->
      precList
        (\self x xs -> plam $ \remAuthSignatories sigs ->
          pif (reqSigs #<= sigs)
            (pcon PTrue)
            (pmatch (pelem' # x # remAuthSignatories) $ \case
              PNothing -> self # xs # remAuthSignatories # sigs
              PJust remAuthSignatories' -> self # xs # remAuthSignatories' # (sigs + 1)
            )
        )
        (\_self -> plam $ \_remReqUniqueSignatories sigs -> reqSigs #<= sigs)
        # txSignatories
        # authorizedSignatores
        # 0

-- | 'toDatum' Converts any 'PType' that has instance of 'PIsData' to 'PDatum'.
toDatum :: forall a s. PIsData a => Term s a -> Term s PDatum
toDatum = pcon . PDatum . pforgetData . pdata

-- | 'pfindTxOutByTxOutRef' searches for 'PTxInInfo' given 'PTxOutRef' and returns 'PTxOut' of the
--   matched 'PTxInInfo', fails if such 'PTxInInfo' is not present.
pfindTxOutByTxOutRef ::
  Term s (PTxOutRef
    :-->  PBuiltinList PV2.PTxInInfo
    :-->  PV2.PTxOut
         )
pfindTxOutByTxOutRef = phoistAcyclic $ plam $ \txOutRef inputs
  -> precList (\self x xs
  ->            pif  (pdata txOutRef #== pfield @"outRef" # x)
                     (pfield @"resolved" # x)
                     (self # xs)
              )
              (const $ ptraceError "There's no TxInInfo that contains given TxOutRef.")
              # inputs

-- errors if the purpose isn't "Minting"
pownSymbol :: Term s (PV2.PScriptContext :--> PCurrencySymbol)
pownSymbol = phoistAcyclic $ plam $ \ctx -> unTermCont $ do
  PMinting csRec <- pmatchC $ pfield @"purpose" # ctx
  pure $ pfield @"_0" # csRec

-- | 'pexpectedTokenName' is the 'psha2_256' hash of 'PTxId' + 'Integer' of the given 'PTxOutRef'.
pexpectedTokenName ::
  Term s (PTxOutRef
    :-->  PTokenName
         )
pexpectedTokenName = phoistAcyclic $ plam $ \txOutRef
  -> pletFields @["idx", "id"] txOutRef $ \v
  -> pcon $ PTokenName $ psha2_256 #$ pconsBS #
            getField @"idx" v # (pfield @"_0" #$ getField @"id" v)

{- | 'ppaidValue' returns the first 'PValue' paid to a particular 'PAddress' in a given 'PTxInfo' output list,
      This done by checking if the given `PAddress` and hash of given datum
      is present in any of the output 'PTxOut' of `PTxInfo`.
      Returns 'mempty' is nothing is paid.
-}
ppaidValue :: PIsData a => Term s (a
  :--> PAddress
  :--> PMap 'PMap.Unsorted PDatumHash PDatum
  :--> PBuiltinList PV2.PTxOut
  :--> PValue 'Sorted 'Positive)
ppaidValue = phoistAcyclic $ ppaidValueCore $ \f ->
  precList
    (\self x xs ->
      pif (f # x)
        (pfield @"value" # x)
        (self # xs)
    )
    (const mempty)

{- | 'ppaidValuePlusInline' returns the first 'PValue' paid to a particular 'PAddress' in a given 'PTxInfo' output list
      with a given datum.
      This is done by checking if the given `PAddress` and given datum
      are present in any of the outputs 'PTxOut' of `PTxInfo`.
      Returns 'mempty' if nothing is paid.

  __NOTE:__ It's difference with respect to `ppaidValue` is that it supports matching against both inlined or non-inlined datum.
-}
ppaidValuePlusInline :: PIsData a
                     => Term s (
                             a
                        :--> PAddress
                        :--> PMap 'PMap.Unsorted PDatumHash PDatum
                        :--> PBuiltinList PV2.PTxOut
                        :--> PValue 'Sorted 'Positive)
ppaidValuePlusInline = phoistAcyclic $ ppaidValueCorePlusInline $ \f ->
  precList
    (\self x xs ->
      pif (f # x)
        (pfield @"value" # x)
        (self # xs)
    )
    (const mempty)

-- | Like 'ppaidValue', but sums up 'PValue's from multiple UTxOs if there are multiple matches.
ppaidValueSum :: PIsData a => Term s (a
  :--> PAddress
  :--> PMap 'PMap.Unsorted PDatumHash PDatum
  :--> PBuiltinList PV2.PTxOut
  :--> PValue 'Sorted 'Positive)
ppaidValueSum = phoistAcyclic $ ppaidValueCore $ \f ->
  pfoldl
    # plam
        (\acc x ->
          pif (f # x)
            (acc <> (pfield @"value" # x))
            acc
        )
    # mempty

ppaidValueCore :: PIsData a
               => (Term s (PV2.PTxOut :--> PBool)
               -> Term s (PBuiltinList PV2.PTxOut :--> PValue 'Sorted 'Positive))
               -> Term s (a
                     :--> PAddress
                     :--> PMap 'PMap.Unsorted PDatumHash PDatum
                     :--> PBuiltinList PV2.PTxOut
                     :--> PValue 'Sorted 'Positive)
ppaidValueCore go = plam $ \ref addr datums -> unTermCont $ do

  expectedPaymentDatumHash <- pletC $
    pdcons
      # (precList
          (\self x' xs -> unTermCont $ do
            dh <- pletC $ pfstBuiltin # x'
            d <- pletC $ psndBuiltin # x'
            pure $ pif (d #== pdata (toDatum ref))
              dh
              (self # xs)
          )
          (const $ ptraceError "Expected the datum to be present in the Tx.")
          # pto datums
        )
      # pdnil

  let f = plam $ \o' -> unTermCont $ do
            o <- pletFieldsC @["datum", "address"] o'
            outDatum <- pletC $ getField @"datum" o
            pure $ outDatum #== pcon (POutputDatumHash expectedPaymentDatumHash)
              #&& getField @"address" o #== addr

  pure $ go f

ppaidValueCorePlusInline :: PIsData a
                         => (Term s (PV2.PTxOut :--> PBool)
                         -> Term s (PBuiltinList PV2.PTxOut :--> PValue 'Sorted 'Positive))
                         -> Term s (a
                               :--> PAddress
                               :--> PMap 'PMap.Unsorted PDatumHash PDatum
                               :--> PBuiltinList PV2.PTxOut
                               :--> PValue 'Sorted 'Positive)
ppaidValueCorePlusInline go = plam $ \ref addr datums -> unTermCont $ do
  commonCheck <- pletC $ plam $ \outDatum outAddr -> outDatum #== toDatum ref #&& outAddr #== addr
  let f = plam $ \o' -> unTermCont $ do
            o <- pletFieldsC @["datum", "address"] o'
            outDatum' <- pletC $ getField @"datum" o
            outAddr <- pletC $ getField @"address" o
            pmatchC outDatum' >>= \case
              PV2.PNoOutputDatum _ -> pure $ pconstant False
              PV2.POutputDatum x   -> do
                outDatum <- pletC (pfield @"outputDatum" # x)
                pure $ commonCheck # outDatum # outAddr
              PV2.POutputDatumHash x -> pmatchC (PMap.plookup # (pfield @"datumHash" # x) # datums) >>= \case
                PNothing       -> pure $ pconstant False
                PJust outDatum -> pure $ commonCheck # outDatum # outAddr
  pure $ go f

pmintedTokens ::
  Term s (PCurrencySymbol
    :-->  PTokenName
    :-->  PV2.PTxInfo
    :-->  PInteger
         )
pmintedTokens = phoistAcyclic $ plam $ \cs tn info
  -> unTermCont $ do
      mint  <- pletC . pto . pfromData $ pfield @"mint" # info

      PJust tnMap <-  pmatchC $ PMap.plookup # cs # mint
      return (pfromMaybe # 0 # (PMap.plookup # tn # tnMap))

pdecodeInlineDatum :: forall a s. (PTryFrom PData (PAsData a), PIsData a)
                   => Term s (PV2.POutputDatum :--> a)
pdecodeInlineDatum = phoistAcyclic $
    plam $ \od ->
        pmatch od $ \case
            PV2.POutputDatum datum ->
                plet (pfield @"outputDatum" # datum) $ \datumResolved ->
                    (pfromData . ptryFromData @a . pto . pfromData) datumResolved
            _                      -> ptraceError "expected inline datum"

presolveInlineDatum :: Term s (PV2.POutputDatum :--> PDatum)
presolveInlineDatum = phoistAcyclic $
  plam $ \od ->
    pmatch od $ \case
      PV2.POutputDatum datum -> (pfield @"outputDatum" # datum)
      _                      -> ptraceError "expected inline datum"

-- | If the datum of output is inlined, we return it else if it contains hash, we try to find the corresponding datum from witness.
presolveDatum ::
  Term s (PV2.POutputDatum
     :--> PMap 'PMap.Unsorted PDatumHash PDatum
     :--> PDatum
         )
presolveDatum =
  phoistAcyclic $
    plam $ \outDatum' datums ->
      pmatch outDatum' $ \case
        PV2.PNoOutputDatum _   -> ptraceError "presolveDatum: Expected datum in this output"
        PV2.POutputDatum x     -> pfield @"outputDatum" # x
        PV2.POutputDatumHash x ->
          pmatch (PMap.plookup # (pfield @"datumHash" # x) # datums) $ \case
            PNothing       -> ptraceError "presolveDatum: Datum not found in witness map corresponding to datum hash present in this output"
            PJust outDatum -> outDatum
