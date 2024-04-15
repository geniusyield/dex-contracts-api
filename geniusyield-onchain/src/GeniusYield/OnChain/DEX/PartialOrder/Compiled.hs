{-# LANGUAGE DataKinds #-}

module GeniusYield.OnChain.DEX.PartialOrder.Compiled (
    originalPartialOrderValidator,
    optimizedPartialOrderValidator,
    optimizedPartialOrderValidatorWithTracing,
) where

import           PlutusLedgerApi.V1
import           PlutusLedgerApi.V1.Value             (AssetClass)

import           Data.Default                         (def)
import           Data.Text                            (Text)
import           GeniusYield.OnChain.DEX.PartialOrder
import           Plutarch
import           Plutarch.Api.V1
import qualified Plutarch.Api.V2                      as PV2
import qualified Plutarch.Unsafe                      as PUNSAFE
import qualified Plutonomy
import           Ply                                  hiding ((#))
import           Ply.Plutarch

import           GeniusYield.OnChain.Plutarch.Api     (PAssetClass)
import           GeniusYield.OnChain.Utils            (desiredTracingMode)
import           GeniusYield.Plutonomy                ()

originalPartialOrderValidator :: Config -> Either Text (TypedScript 'ValidatorRole '[Address, AssetClass])
originalPartialOrderValidator cnf = toTypedScript cnf mkPartialOrderValidator'

optimizedPartialOrderValidator :: Either
  Text
  (TypedScript 'ValidatorRole '[Address, AssetClass])
optimizedPartialOrderValidator = Plutonomy.optimizeUPLC <$> originalPartialOrderValidator def

optimizedPartialOrderValidatorWithTracing :: Either
  Text
  (TypedScript 'ValidatorRole '[Address, AssetClass])
optimizedPartialOrderValidatorWithTracing = Plutonomy.optimizeUPLC <$> originalPartialOrderValidator def {tracingMode = desiredTracingMode}

mkPartialOrderValidator' ::
  ClosedTerm ( PAddress
          :--> PAssetClass
          :--> PV2.PValidator
             )
mkPartialOrderValidator' = plam $ \refInputAddr refInputToken datm redm ctx ->
  popaque $ mkPartialOrderValidator
    # refInputAddr
    # refInputToken
    # PUNSAFE.punsafeCoerce datm
    # PUNSAFE.punsafeCoerce redm
    # ctx
