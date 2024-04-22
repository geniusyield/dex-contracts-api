{-# LANGUAGE DataKinds #-}

module GeniusYield.OnChain.Plutarch.Types where

import           Plutarch.Api.V1
import           Plutarch.DataRepr
import           Plutarch.Extra.RationalData (PRationalData)
import           Plutarch.Prelude
import           PlutusLedgerApi.V1.Value    (AssetClass)
import qualified PlutusTx.Ratio              as PlutusTx
import           Ply.Plutarch.Class          (PlyArgOf)

{- | 'PAssetClass' is the plutarch level type for 'AssetClass' defined in "Ledger.Value".
-}
newtype PAssetClass (s :: S)
  = PAssetClass
      ( Term
          s
          ( PDataRecord
              '[ "currencySymbol" ':= PCurrencySymbol
               , "tokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields, PTryFrom PData)

instance DerivePlutusType PAssetClass where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PAssetClass)

type instance PlyArgOf PAssetClass = AssetClass

newtype Flip f a b = Flip (f b a)
  deriving stock (Generic)

type instance PlyArgOf PRationalData = PlutusTx.Rational
