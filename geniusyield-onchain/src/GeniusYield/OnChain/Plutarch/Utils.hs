{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module GeniusYield.OnChain.Plutarch.Utils ( pfromMaybe
                                          , ptoRational
                                          , pceiling
                                          , pfloor
                                          , plookupTuple
                                          , pmin
                                          , ppairToRat
                                          , pgetContinuingOutputUsingNft
                                          , pgetContinuingOutputs
                                          , pfindOwnInput
                                          , pfindOutputWithNft
                                          , pisAddressForScript
                                          , pparseDatum
                                          , pparseDatum'
                                          , ptryFromData
                                          , pelem'
                                          , pallUnique
                                          ) where

import           GeniusYield.OnChain.Plutarch.Types (PAssetClass (..))
import           GeniusYield.OnChain.Plutarch.Value (passetClassValueOf)
import           Plutarch.Api.V1                    (KeyGuarantees (Unsorted), PAddress, PCredential (..), PDatum,
                                                     PDatumHash, PMap, PTuple, PTxOutRef)
import qualified Plutarch.Api.V1.AssocMap           as PMap
import           Plutarch.Api.V1.Scripts            (PScriptHash)
import qualified Plutarch.Api.V2                    as PV2
import           Plutarch.Extra.Maybe               (pjust)
import           Plutarch.Num
import           Plutarch.Positive                  (ptryPositive)
import           Plutarch.Prelude                   hiding (psingleton)
import qualified Plutarch.Rational                  as PRational

-- $setup
--
-- >>> :set -XDataKinds
-- >>> import           GeniusYield.OnChain.Plutarch.Run
-- >>> import           Plutarch.Prelude
-- >>> import           Plutarch.Extra.Maybe               (pjust, pnothing)

-- | 'pfromMaybe' is the plutarch level function that is similar to 'fromMaybe'.
pfromMaybe :: forall a s. Term s (a :--> PMaybe a :--> a)
pfromMaybe = phoistAcyclic $ plam $ \e a
  -> pmatch a $ \case
      PJust a' -> a'
      PNothing -> e

-- | 'pceiling' is the plutarch level function of 'ceiling'.
pceiling ::
  Term s (PRational
    :-->  PInteger
         )
pceiling = phoistAcyclic $ plam $ \x
  -> plet (PRational.pfromInteger #$ PRational.ptruncate # x) $ \y
  -> plet (PRational.ptruncate # x) $ \z
  -> pif
      (x #< 0)
      z
      (pif
        (x #== y)
        z
        (1 + z)
      )

-- | 'pfloor' is the plutarch level function of 'floor'.
--
-- >>> evalT $ pfloor # pcon (PRational 2 1)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 2))}},ExBudget {exBudgetCPU = ExCPU 3590775, exBudgetMemory = ExMemory 9708},[])
--
-- >>> evalT $ pfloor # pcon (PRational 14 7)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 2))}},ExBudget {exBudgetCPU = ExCPU 3590775, exBudgetMemory = ExMemory 9708},[])
--
-- >>> evalT $ pfloor # pcon (PRational 25 10)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 2))}},ExBudget {exBudgetCPU = ExCPU 3590775, exBudgetMemory = ExMemory 9708},[])
--
-- >>> evalT $ pfloor # pcon (PRational 0 10)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 0))}},ExBudget {exBudgetCPU = ExCPU 3590775, exBudgetMemory = ExMemory 9708},[])
--
-- >>> evalT $ pfloor # pcon (PRational (-1) 10)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger (-1)))}},ExBudget {exBudgetCPU = ExCPU 5606010, exBudgetMemory = ExMemory 14618},[])
--
-- >>> evalT $ pfloor # pcon (PRational (-4) 2)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger (-2)))}},ExBudget {exBudgetCPU = ExCPU 5307533, exBudgetMemory = ExMemory 14216},[])
--
-- >>> evalT $ pfloor # pcon (PRational (-17) 7)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger (-3)))}},ExBudget {exBudgetCPU = ExCPU 5606010, exBudgetMemory = ExMemory 14618},[])
--
pfloor ::
  Term s (PRational
    :-->  PInteger
         )
pfloor = phoistAcyclic $ plam $ \x
  -> plet (PRational.pfromInteger #$ PRational.ptruncate # x) $ \qtR
  -> plet (PRational.ptruncate # x) $ \qt
  -> pif (x #< 0)
      (pif
        (x #== qtR)
        qt
        (qt - 1)
      )
      qt

-- | 'pfind' that works with a Haskell level predicate.
pfind' ::
  PIsListLike list a =>
  (Term s a -> Term s PBool) ->
  Term s (list a :--> PMaybe a)
pfind' p = precList
    (\self x xs
      -> pif (p x) (pcon (PJust x)) (self # xs)
    )
    (const $ pcon PNothing)

-- | Find the value for a given key in an assoclist which uses 'PTuple's.
plookupTuple ::
  (PEq a, PIsListLike list (PAsData (PTuple a b)), PIsData a, PIsData b) =>
  Term s (a :--> list (PAsData (PTuple a b)) :--> PMaybe b)
plookupTuple = phoistAcyclic $ plam $ \k xs
  -> pmatch (pfind' (\p -> (pfield @"_0" # pfromData p) #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p  -> pcon (PJust (pfield @"_1" # pfromData p))

-- | returns minimum of the two values.
pmin :: (POrd a) =>
  Term s ( a
    :-->   a
    :-->   a
         )
pmin = phoistAcyclic $ plam $ \x y -> pif (x #<= y) x y

-- | converts a 'PBuiltinPair' with 'PInteger' and 'PPositive' into 'PRational'.
ppairToRat :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger) :--> PRational)
ppairToRat = phoistAcyclic $ plam $ \p -> pcon $ PRational (pfromData $ pfstBuiltin # p) (ptryPositive #$ pfromData $ psndBuiltin # p)

ptoRational :: Integer -> Integer -> Term s PRational
ptoRational x y = pcon $ PRational (pfromInteger x) (pfromInteger y)

pgetContinuingOutputUsingNft :: Term s (PAddress :--> PAssetClass :--> PBuiltinList PV2.PTxOut :--> PV2.PTxOut)
pgetContinuingOutputUsingNft = phoistAcyclic $
  plam $ \addressIn nftAC outputs ->
    pmatch (pfind # (matches # addressIn # nftAC) # outputs) $ \case
      PNothing -> ptraceError "no continuing output found"
      PJust o -> o
  where
    matches :: Term s (PAddress :--> PAssetClass :--> PV2.PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \addressIn nftAC txOut ->
        pletFields @'["address", "value"] txOut $ \txOut' ->
          (passetClassValueOf # getField @"value" txOut' # nftAC #== 1) #&& (addressIn #== getField @"address" txOut')

-- V2 versions of some API utils from upstream.

pgetContinuingOutputs :: Term s (PBuiltinList PV2.PTxInInfo :--> PBuiltinList PV2.PTxOut :--> PTxOutRef :--> PBuiltinList PV2.PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \inputs outputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> do
        let resolved = pfield @"resolved" # tx
            outAddr = pfield @"address" # resolved
        pfilter # (matches # outAddr) # outputs
      PNothing ->
        ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PV2.PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

pfindOwnInput :: Term s (PBuiltinList PV2.PTxInInfo :--> PTxOutRef :--> PMaybe PV2.PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches :: Term s (PTxOutRef :--> PV2.PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

-- | Finds the first output that contains the given NFT.
pfindOutputWithNft :: Term s (PBuiltinList PV2.PTxOut :--> PAssetClass :--> PMaybe PV2.PTxOut)
pfindOutputWithNft = phoistAcyclic $
    plam $ \outputs reqAsset ->
        pfind # (matches # reqAsset) # outputs
      where
        matches :: Term s (PAssetClass :--> PV2.PTxOut :--> PBool)
        matches = phoistAcyclic $
            plam $ \reqAsset txOut ->
                passetClassValueOf # (pfield @"value" # txOut) # reqAsset #== 1

-- | Checks whether the given address belongs to the script with the given hash.
pisAddressForScript :: Term s (PAddress :--> PScriptHash :--> PBool)
pisAddressForScript = phoistAcyclic $
    plam $ \addr scrHash ->
        pmatch (pfield @"credential" # addr) $ \case
            PPubKeyCredential _ -> pconstant False
            PScriptCredential x -> scrHash #== pfield @"_0" # x

pparseDatum :: forall a s. PTryFrom PData (PAsData a)
            => Term s (PDatumHash
                  :--> PMap 'Unsorted PDatumHash PDatum
                  :--> PMaybe (PAsData a)
                      )
pparseDatum = phoistAcyclic $
    plam $ \dh datums ->
        pmatch (PMap.plookup # dh # datums) $ \case
            PNothing   -> pcon PNothing
            PJust datm -> pcon . PJust . ptryFromData $ pto datm

pparseDatum' :: forall a s. PTryFrom PData (PAsData a)
             => Term s (PV2.POutputDatum
                   :--> PMap 'Unsorted PDatumHash PDatum
                   :--> PAsData a
                       )
pparseDatum' = phoistAcyclic $
  plam $ \od datums ->
    pmatch od $ \case
        PV2.PNoOutputDatum _   -> ptraceError "expected output datum"
        PV2.POutputDatum x     -> plet (pfield @"outputDatum" # x) $
            \(datm :: Term _ PDatum) ->
                ptryFromData $ pto datm
        PV2.POutputDatumHash x -> pmatch (PMap.plookup # (pfield @"datumHash" # x) # datums) $ \case
            PNothing   -> ptraceError "output datum not found"
            PJust datm -> ptryFromData (pto datm)

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)


-- | / O(n) /. Like `pelem` but returns the list without the element if it is found.
--
-- >>> evalT $ (pelem' # (1 :: Term s PInteger) #$ pcons @PList # 1 #$ pcons # 2 #$ pcons # 3 # pnil) #== (pjust #$ pcons @PList # 2 #$ pcons # 3 # pnil)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 6526567, exBudgetMemory = ExMemory 24706},[])
--
-- >>> evalT $ (pelem' # (2 :: Term s PInteger) #$ pcons @PList # 1 #$ pcons # 2 #$ pcons # 3 # pnil) #== (pjust #$ pcons @PList # 1 #$ pcons # 3 # pnil)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 8058056, exBudgetMemory = ExMemory 30108},[])
--
-- >>> evalT $ (pelem' # (3 :: Term s PInteger) #$ pcons @PList # 1 #$ pcons # 2 #$ pcons # 3 # pnil) #== (pjust #$ pcons @PList # 1 #$ pcons # 2 # pnil)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 9589545, exBudgetMemory = ExMemory 35510},[])
--
-- >>> evalT $ (pelem' # (4 :: Term s PInteger) #$ pcons @PList # 1 #$ pcons # 2 #$ pcons # 3 # pnil) #== pnothing
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 5675567, exBudgetMemory = ExMemory 21006},[])
--
-- >>> evalT $ (pelem' # (4 :: Term s PInteger) # pcon PSNil) #== pnothing
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 1288100, exBudgetMemory = ExMemory 5700},[])
--
-- >>> evalT $ (pelem' # (1 :: Term s PInteger) #$ pcons @PList # 1 # pnil) #== pjust # pnil
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 3049589, exBudgetMemory = ExMemory 12102},[])
--
pelem' :: (PIsListLike list a, PEq a) => Term s (a :--> list a :--> PMaybe (list a))
pelem' =
  phoistAcyclic $
    plam $ \needle ->
      precList
        (\self x xs ->
          pif (x #== needle)
              (pjust # xs)
              (pmatch (self # xs) $ \case
                PNothing -> pcon PNothing
                PJust xs' -> pjust #$ pcons # x # xs'
              )
        )
        (\_self -> pcon PNothing)

-- | Check if all the elements in the list are unique.
--
-- >>> evalT $ (pallUnique #$ pcons @PList # (1 :: Term s PInteger) #$ pcons # 2 #$ pcons # 3 # pnil) #== (pcon PTrue)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 8815347, exBudgetMemory = ExMemory 32911},[])
--
-- >>> evalT $ (pallUnique #$ (pnil :: Term s (PList PInteger))) #== (pcon PTrue)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 1150212, exBudgetMemory = ExMemory 4402},[])
--
-- >>> evalT $ (pallUnique #$ pcons @PList # (1 :: Term s PInteger) #$ pcons # 2 #$ pcons # 3 #$ pcons # 2 # pnil) #== (pcon PFalse)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniBool True))}},ExBudget {exBudgetCPU = ExCPU 8991769, exBudgetMemory = ExMemory 31514},[])
--
pallUnique :: (PIsListLike list a, PEq a) => Term s (list a :--> PBool)
pallUnique = phoistAcyclic $
  precList
    (\self x xs -> pif (pelem # x # xs) (pcon PFalse) (self # xs))
    (const $ pcon PTrue)
