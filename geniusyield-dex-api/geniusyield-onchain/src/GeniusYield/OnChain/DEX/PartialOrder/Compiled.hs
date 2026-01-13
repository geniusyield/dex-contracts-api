{-# LANGUAGE DataKinds #-}

module GeniusYield.OnChain.DEX.PartialOrder.Compiled (
  originalPartialOrderValidator,
  optimizedPartialOrderValidator,
  optimizedPartialOrderValidatorWithTracing,
) where

import Data.Default (def)
import Data.Text (Text)
import GeniusYield.OnChain.DEX.PartialOrder
import GeniusYield.OnChain.Plutarch.Api (PAssetClass)
import GeniusYield.OnChain.Utils (desiredTracingMode)
import GeniusYield.Plutonomy ()
import Plutarch
import Plutarch.Api.V1
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Unsafe qualified as PUNSAFE
import Plutonomy qualified
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply hiding ((#))
import Ply.Plutarch

originalPartialOrderValidator :: Config -> Either Text (TypedScript 'ValidatorRole '[Address, AssetClass])
originalPartialOrderValidator cnf = toTypedScript cnf mkPartialOrderValidator'

optimizedPartialOrderValidator
  :: Either
       Text
       (TypedScript 'ValidatorRole '[Address, AssetClass])
optimizedPartialOrderValidator = Plutonomy.optimizeUPLC <$> originalPartialOrderValidator def

optimizedPartialOrderValidatorWithTracing
  :: Either
       Text
       (TypedScript 'ValidatorRole '[Address, AssetClass])
optimizedPartialOrderValidatorWithTracing = Plutonomy.optimizeUPLC <$> originalPartialOrderValidator def {tracingMode = desiredTracingMode}

mkPartialOrderValidator'
  :: ClosedTerm
       ( PAddress
           :--> PAssetClass
           :--> PV2.PValidator
       )
mkPartialOrderValidator' = plam $ \refInputAddr refInputToken datm redm ctx ->
  popaque $
    mkPartialOrderValidator
      # refInputAddr
      # refInputToken
      # PUNSAFE.punsafeCoerce datm
      # PUNSAFE.punsafeCoerce redm
      # ctx
