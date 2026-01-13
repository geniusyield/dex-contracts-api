{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Scripts.DEX.PartialOrder (
  -- * Validator
  partialOrderValidator,
  partialOrderValidatorHash,
  PartialOrderFeeOutput (..),
  PartialOrderContainedFee (..),

  -- * Datum
  PartialOrderDatum (..),

  -- * Redeemer
  PartialOrderAction (..),
) where

import GeniusYield.OnChain.Core.Common.DEX.PartialOrder
import GeniusYield.Scripts.DEX.PartialOrderConfig (POCVersion, partialOrderConfigPlutusAddr)
import GeniusYield.Scripts.Internal
import GeniusYield.Types
import Ply ((#))

partialOrderValidator :: GYCompiledScriptsRaw -> POCVersion -> GYAssetClass -> GYScript PlutusV2
partialOrderValidator gycs@GYCompiledScriptsRaw {gycsDEXPartialOrder} pocVersion ac =
  validatorFromPly $
    gycsDEXPartialOrder
      # partialOrderConfigPlutusAddr gycs pocVersion ac
      # assetClassToPlutus ac

partialOrderValidatorHash
  :: GYCompiledScriptsRaw
  -> POCVersion
  -> GYAssetClass
  -> GYScriptHash
partialOrderValidatorHash gycs pocVersion = validatorHash . partialOrderValidator gycs pocVersion
