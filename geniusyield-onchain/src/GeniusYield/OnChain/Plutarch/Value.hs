{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module GeniusYield.OnChain.Plutarch.Value
    ( psubtractValue
    , passetClassValue
    , passetClassValuePositive
    , psingletonPositiveData
    , passetClassValueOf
    , pisNonNegative
    , passetClass
    , plovelace
    , pgeq
    , pleq
    , pvalTotalEntries
    ) where

import           Plutarch.Api.V1                    (AmountGuarantees (..), KeyGuarantees (..), PCurrencySymbol,
                                                     PTokenName, PValue)
import qualified Plutarch.Api.V1.AssocMap           as AssocMap
import qualified Plutarch.Api.V1.Value              as PValue
import           Plutarch.Extra.TermCont            (pletC)
import qualified Plutarch.List                      as List
import           Plutarch.Prelude
import           PlutusLedgerApi.V1.Value           (adaSymbol, adaToken)

import           GeniusYield.OnChain.Plutarch.Types (PAssetClass (..))
import qualified Plutarch.Api.V1.AssocMap           as PMap
import qualified Plutarch.Unsafe                    as PUNSAFE
import qualified PlutusTx.Monoid

-- $setup
--
-- >>> :set -XOverloadedStrings -XDataKinds
-- >>> import qualified GeniusYield.OnChain.Plutarch.Run as Run
-- >>> import qualified PlutusLedgerApi.V1.Value as PlutusTx
-- >>> import Plutarch.Prelude
-- >>> import Plutarch.Api.V1
-- >>> import qualified PlutusTx
-- >>> import Plutarch.Unsafe (punsafeCoerce)
--
-- >>> let tokenACS :: PlutusTx.CurrencySymbol = "12345678912345678912345678912345678912345678912345678912"
-- >>> let tokenAN :: PlutusTx.TokenName = "tokenA"
-- >>> let tokenA = PlutusTx.assetClass tokenACS tokenAN
--
-- >>> let tokenBCS :: PlutusTx.CurrencySymbol = "12345678912345678912345678912345678912345678912345678913"
-- >>> let tokenBN :: PlutusTx.TokenName = "tokenB"
-- >>> let tokenB = PlutusTx.assetClass tokenBCS tokenBN
--
-- >>> let valA = PlutusTx.assetClassValue tokenA 123456789
-- >>> let valA' = PlutusTx.assetClassValue (PlutusTx.assetClass tokenACS "tokenA'") 123456789
-- >>> let valB = PlutusTx.assetClassValue tokenB 123456789
--
-- >>> :{
--   f :: Term s (PValue 'Sorted 'NoGuarantees :--> PValue 'Sorted 'NoGuarantees :--> PValue 'Sorted 'NoGuarantees)
--   f = phoistAcyclic $ plam $ \v1 v2 -> v1 <> v2
-- :}
--
-- To measure memory consumption of joining values, compared to joining two @PPartialOrderContainedFee@.
--
-- >>> Run.evalT $ f # (punsafeCoerce (pconstant valA)) # (punsafeCoerce (pconstant valB))
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) [(B "\DC24Vx\145#Eg\137\DC24Vx\145#Eg\137\DC24Vx\145#Eg\137\DC2",Map [(B "tokenA",I 123456789)]),(B "\DC24Vx\145#Eg\137\DC24Vx\145#Eg\137\DC24Vx\145#Eg\137\DC3",Map [(B "tokenB",I 123456789)])]))}},ExBudget {exBudgetCPU = ExCPU 8204258, exBudgetMemory = ExMemory 25020},[])

-- FIXME: Eventually there should be a utility function in upstream Plutarch
--        for casting values to NoGuarantees. When that function exists,
--        replace the PUNSAFE.punsafeCoerce with it
psubtractValue :: Term s
          ( PValue 'Sorted anyG
    :-->    PValue 'Sorted anyG
    :-->    PValue 'Sorted 'NoGuarantees)
psubtractValue = phoistAcyclic $ plam $ \v1 v2 ->
  PValue.punionWith # plam (+)
   # v1
   # PlutusTx.Monoid.inv (PUNSAFE.punsafeCoerce v2 :: Term _ (PValue 'Sorted 'NoGuarantees))

{- | 'passetClassValueOf' is the plutarch level function that is similar to 'assetClassValueOf'
      defined in "Ledger". 'passetClassValueOf' returns 'PInteger' if the given 'PAssetClass'
      is present in the 'PValue', else return 0.
-}
passetClassValueOf ::
  Term s (PValue anyK anyG
    :-->  PAssetClass
    :-->  PInteger
         )
passetClassValueOf = phoistAcyclic $ plam $ \v ac -> unTermCont $ do
  cs <- pletC $ pfield @"currencySymbol" # ac
  tn <- pletC $ pfield @"tokenName" # ac
  pure $ PValue.pvalueOf # v # cs # tn

-- | creates a singleton value from the 'PAssetClass' and it's amount.
passetClassValue ::
  Term s ( PAssetClass
    :-->   PAsData PInteger
    :-->   PValue 'Sorted 'NonZero
         )
passetClassValue = phoistAcyclic $ plam $ \ac v
  -> unTermCont $ do

     cs <- pletC $ pfield @"currencySymbol" # ac
     tn <- pletC $ pfield @"tokenName" # ac
     pure $ PValue.psingletonData # cs # tn # v

-- | Creates a singleton value from the 'PAssetClass' and it's amount, where amount is checked to be positive.
passetClassValuePositive ::
  Term s ( PAssetClass
    :-->   PAsData PInteger
    :-->   PValue 'Sorted 'Positive
         )
passetClassValuePositive = phoistAcyclic $ plam $ \ac v
  -> unTermCont $ do

     cs <- pletC $ pfield @"currencySymbol" # ac
     tn <- pletC $ pfield @"tokenName" # ac
     pure $ psingletonPositiveData # cs # tn # v

-- | Construct a singleton 'PValue' containing only the given positive quantity of the given currency, taking data-encoded parameters. Implementation is a slightly modified version of @psingletonData@ defined [here](https://github.com/Plutonomicon/plutarch-plutus/blob/c14ad83479706566fe22e7b7b50b696043326c8f/Plutarch/Api/V1/Value.hs#L283-L305).
--
-- >>> Run.evalWithArgsT psingletonPositiveData [PlutusTx.toData tokenACS, PlutusTx.toData tokenAN, PlutusTx.toData 2]
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) [(B "\DC24Vx\145#Eg\137\DC24Vx\145#Eg\137\DC24Vx\145#Eg\137\DC2",Map [(B "tokenA",I 2)])]))}},ExBudget {exBudgetCPU = ExCPU 2173250, exBudgetMemory = ExMemory 6794},[])
--
-- >>> Run.evalWithArgsT psingletonPositiveData [PlutusTx.toData tokenACS, PlutusTx.toData tokenAN, PlutusTx.toData (-2)]
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) []))}},ExBudget {exBudgetCPU = ExCPU 1134410, exBudgetMemory = ExMemory 3634},[])
--
psingletonPositiveData ::
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PAsData PInteger
        :--> PValue 'Sorted 'Positive
    )
psingletonPositiveData = phoistAcyclic $
  plam $ \symbol token amount ->
    pif
      (pfromData amount #<= 0)
      mempty
      ( PUNSAFE.punsafeDowncast
          ( AssocMap.psingletonData
              # symbol
                #$ pdata
              $ AssocMap.psingletonData # token # amount
          )
      )

-- | Check if a 'PValue' is non negative (all amounts are greater than equal to 0).
pisNonNegative :: forall any s. (forall s'. Semigroup (Term s' (PValue 'Sorted any))) => Term s (PValue 'Sorted any :--> PBool)
pisNonNegative = phoistAcyclic $ plam $ \x -> PMap.pall # plam (\submap -> PMap.pall # plam (0 #<=) # submap) # pto x

passetClass :: Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
passetClass = phoistAcyclic $
    plam $ \cs tk ->
        pcon $ PAssetClass $ pdcons @"currencySymbol" # pdata cs #$ pdcons @"tokenName" # pdata tk # pdnil

plovelace :: ClosedTerm PAssetClass
plovelace = passetClass # pconstant adaSymbol # pconstant adaToken

pgeq :: Term s (PValue 'Sorted any :--> PValue 'Sorted any :--> PBool)
pgeq = phoistAcyclic $ plam $ \v1 v2 -> pisNonNegative #$ psubtractValue # v1 # v2

pleq :: Term s (PValue 'Sorted any :--> PValue 'Sorted any :--> PBool)
pleq = phoistAcyclic $ plam $ \v1 v2 -> pgeq # v2 # v1

-- | Gives the total entries present in the value. Note that logic of this function requires @Positive@ guarantee.
--
-- >>> Run.evalT $ pvalTotalEntries # mempty
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 0))}},ExBudget {exBudgetCPU = ExCPU 1141454, exBudgetMemory = ExMemory 4332},[])
--
-- >>> Run.evalT $ pvalTotalEntries # punsafeCoerce (pconstant valA)
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 1))}},ExBudget {exBudgetCPU = ExCPU 4538328, exBudgetMemory = ExMemory 14192},[])
--
-- >>> Run.evalT $ pvalTotalEntries # punsafeCoerce (pconstant (valA <> valB))
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 2))}},ExBudget {exBudgetCPU = ExCPU 7935202, exBudgetMemory = ExMemory 24052},[])
--
-- >>> Run.evalT $ pvalTotalEntries # punsafeCoerce (pconstant (valA <> valA' <> valB))
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniInteger 3))}},ExBudget {exBudgetCPU = ExCPU 9025215, exBudgetMemory = ExMemory 27018},[])
--
pvalTotalEntries :: Term s (PValue 'Sorted 'Positive :--> PInteger)
pvalTotalEntries = phoistAcyclic $ plam $ \v -> List.pfoldl # f # 0 # pto (pto v)
  where
    f = plam $ \acc tokenMap' -> acc + (List.plength #$ pto (pfromData $ psndBuiltin # tokenMap'))
