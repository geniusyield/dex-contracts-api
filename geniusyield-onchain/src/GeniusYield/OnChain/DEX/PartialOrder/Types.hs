{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module GeniusYield.OnChain.DEX.PartialOrder.Types
    ( PMaybePPOSIXTimeData (..)
    , PPartialOrderFeeOutput (..)
    , PPartialOrderContainedFee (..)
    , PPartialOrderConfigDatum (..)
    , PPartialOrderDatum (..)
    , PPartialOrderAction (..)
    , type PHasType
    , type PartialOrderFeeOutputRec
    , type PartialOrderContainedFeeRec
    , type PartialOrderConfigRec
    , type PartialOrderRec
    ) where

import           GHC.TypeLits
import           Plutarch
import           Plutarch.Api.V1
import           Plutarch.DataRepr
import           Plutarch.DataRepr.Internal
import           Plutarch.DataRepr.Internal.HList.Utils
import           Plutarch.Extra.RationalData
import           Plutarch.Prelude

import           GeniusYield.OnChain.Plutarch.Types

-- $setup
-- >>> :set -XDataKinds
-- >>> import           GeniusYield.OnChain.Plutarch.Run
-- >>> import           Plutarch.Prelude

type PHasType l a fs =
    (PUnLabel (IndexList (PLabelIndex l fs) fs) ~ a, KnownSymbol l, KnownNat (PLabelIndex l fs))

type PartialOrderFeeOutputRec
  = '[ "pofdMentionedFees" ':= PMap 'Unsorted PTxOutRef (PValue 'Sorted 'Positive)
     , "pofdReservedValue" ':= PValue 'Sorted 'Positive
     , "pofdSpentUTxORef"  ':= PMaybeData PTxOutRef  -- Here we don't require @PAsData@ wrapper to get @PTryFrom@ instance, i.e., we don't need to put @PMaybeData (PAsData PTxOutRef)@ unlike for @PPOSIXTime@.
     ]

newtype PPartialOrderFeeOutput (s :: S)
  = PPartialOrderFeeOutput (Term s (PDataRecord PartialOrderFeeOutputRec))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields, PTryFrom PData)

instance DerivePlutusType PPartialOrderFeeOutput where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PPartialOrderFeeOutput)

type PartialOrderContainedFeeRec
  = '[ "pocfLovelaces"     ':= PInteger
     , "pocfOfferedTokens" ':= PInteger
     , "pocfAskedTokens"   ':= PInteger
     ]

newtype PPartialOrderContainedFee (s :: S)
  = PPartialOrderContainedFee (Term s (PDataRecord PartialOrderContainedFeeRec))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, PDataFields, PTryFrom PData)

instance DerivePlutusType PPartialOrderContainedFee where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PPartialOrderContainedFee)

-- |
-- >>> :{
--   let a =
--         pcon $ PPartialOrderContainedFee
--           $  pdcons @"pocfLovelaces" # pdata 1
--           #$ pdcons @"pocfOfferedTokens" # pdata 2
--           #$ pdcons @"pocfAskedTokens" # pdata 3
--           #$ pdnil
--       b =
--         pcon $ PPartialOrderContainedFee
--           $  pdcons @"pocfLovelaces" # pdata 2
--           #$ pdcons @"pocfOfferedTokens" # pdata 1
--           #$ pdcons @"pocfAskedTokens" # pdata 0
--           #$ pdnil
--   in (evalT $ a <> b, evalT $ b <> mempty)
-- :}
-- (Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniData (Constr 0 [I 3,I 3,I 3])))}},ExBudget {exBudgetCPU = ExCPU 6087009, exBudgetMemory = ExMemory 17418},[]),Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf DefaultUniData (Constr 0 [I 2,I 1,I 0])))}},ExBudget {exBudgetCPU = ExCPU 6062009, exBudgetMemory = ExMemory 17254},[]))

instance Semigroup (Term s PPartialOrderContainedFee) where
  a <> b =
    pletFields @["pocfLovelaces", "pocfOfferedTokens", "pocfAskedTokens"] a $ \a' ->
    pletFields @["pocfLovelaces", "pocfOfferedTokens", "pocfAskedTokens"] b $ \b' ->
    pcon $ PPartialOrderContainedFee
      $  pdcons @"pocfLovelaces" # pdata (getField @"pocfLovelaces" a' + getField @"pocfLovelaces" b')
      #$ pdcons @"pocfOfferedTokens" # pdata (getField @"pocfOfferedTokens" a' + getField @"pocfOfferedTokens" b')
      #$ pdcons @"pocfAskedTokens" # pdata (getField @"pocfAskedTokens" a' + getField @"pocfAskedTokens" b')
      #$ pdnil

instance Monoid (Term s PPartialOrderContainedFee) where
  mempty =
    plet (pdata 0) $ \z ->
    pcon $ PPartialOrderContainedFee
      $  pdcons @"pocfLovelaces" # z
      #$ pdcons @"pocfOfferedTokens" # z
      #$ pdcons @"pocfAskedTokens" # z
      #$ pdnil

type PartialOrderConfigRec
  = '[ "pocdSignatories"    ':= PBuiltinList (PAsData PPubKeyHash)
     , "pocdReqSignatories" ':= PInteger
     , "pocdNftSymbol"      ':= PCurrencySymbol
     , "pocdFeeAddr"        ':= PAddress
     , "pocdMakerFeeFlat"   ':= PInteger
     , "pocdMakerFeeRatio"  ':= PRationalData
     , "pocdTakerFee"       ':= PInteger
     , "pocdMinDeposit"     ':= PInteger
     ]

newtype PPartialOrderConfigDatum s
  = PPartialOrderConfigDatum (Term s (PDataRecord PartialOrderConfigRec))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PPartialOrderConfigDatum where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PPartialOrderConfigDatum)

type PartialOrderRec
  = '[ "podOwnerKey"               ':= PPubKeyHash
     , "podOwnerAddr"              ':= PAddress
     , "podOfferedAsset"           ':= PAssetClass
     , "podOfferedOriginalAmount"  ':= PInteger
     , "podOfferedAmount"          ':= PInteger
     , "podAskedAsset"             ':= PAssetClass
     , "podPrice"                  ':= PRationalData
     , "podNFT"                    ':= PTokenName
     , "podStart"                  ':= PMaybePPOSIXTimeData
     , "podEnd"                    ':= PMaybePPOSIXTimeData
     , "podPartialFills"           ':= PInteger
     , "podMakerLovelaceFlatFee"   ':= PInteger
     , "podTakerLovelaceFlatFee"   ':= PInteger
     , "podContainedFee"           ':= PPartialOrderContainedFee
     , "podContainedPayment"       ':= PInteger
     ]

newtype PPartialOrderDatum s
  = PPartialOrderDatum (Term s (PDataRecord PartialOrderRec))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PPartialOrderDatum where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PPartialOrderDatum)

-- | Plutarch's `PMaybeData` but specialised to `PPOSIXTime`. This was done to have `PTryFrom PData PMaybePPOSIXTimeData` instance as otherwise we were having instance of `PTryFrom PData (PMaybeData (PAsData PPOSIXTime))`, i.e., needed to add an extra `PAsData` wrapper around `PPOSIXTime`.
data PMaybePPOSIXTimeData (s :: S)
  = PPDJust (Term s (PDataRecord '["_0" ':= PPOSIXTime]))
  | PPDNothing (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)
instance DerivePlutusType PMaybePPOSIXTimeData where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PMaybePPOSIXTimeData
instance PTryFrom PData (PAsData PMaybePPOSIXTimeData)

data PPartialOrderAction (s :: S)
  = PPartialCancel (Term s (PDataRecord '[]))
  | PPartialFill (Term s (PDataRecord '["_0" ':= PInteger]))
  | PCompleteFill (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PPartialOrderAction where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PPartialOrderAction)
