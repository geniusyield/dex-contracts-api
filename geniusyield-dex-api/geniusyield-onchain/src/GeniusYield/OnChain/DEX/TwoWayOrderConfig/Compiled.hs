{-# LANGUAGE DataKinds #-}

module GeniusYield.OnChain.DEX.TwoWayOrderConfig.Compiled (
  originalTwoWayOrderConfigValidator,
  optimizedTwoWayOrderConfigValidator,
  optimizedTwoWayOrderConfigValidatorWithTracing,
) where

import Data.Default (def)
import Data.Text (Text)
import GeniusYield.OnChain.DEX.TwoWayOrderConfig
import GeniusYield.OnChain.Plutarch.Api (PAssetClass)
import GeniusYield.OnChain.Utils (desiredTracingMode)
import GeniusYield.Plutonomy ()
import Plutarch
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Unsafe qualified as PUNSAFE
import Plutonomy qualified
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply hiding ((#))
import Ply.Plutarch

type POConfigScript = TypedScript 'ValidatorRole '[AssetClass]

originalTwoWayOrderConfigValidator :: Config -> Either Text POConfigScript
originalTwoWayOrderConfigValidator cnf = toTypedScript cnf mkTwoWayOrderConfigValidator'

optimizedTwoWayOrderConfigValidator :: Either Text POConfigScript
optimizedTwoWayOrderConfigValidator = Plutonomy.optimizeUPLC <$> originalTwoWayOrderConfigValidator def

optimizedTwoWayOrderConfigValidatorWithTracing :: Either Text POConfigScript
optimizedTwoWayOrderConfigValidatorWithTracing = Plutonomy.optimizeUPLC <$> originalTwoWayOrderConfigValidator def {tracingMode = desiredTracingMode}

mkTwoWayOrderConfigValidator'
  :: ClosedTerm
       ( PAssetClass
           :--> PV2.PValidator
       )
mkTwoWayOrderConfigValidator' = plam $ \nftAC datm redm ctx ->
  popaque $
    mkTwoWayOrderConfigValidator
      # nftAC
      # PUNSAFE.punsafeCoerce datm
      # PUNSAFE.punsafeCoerce redm
      # ctx
