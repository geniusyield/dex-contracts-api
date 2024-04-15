{-# LANGUAGE DataKinds #-}

module GeniusYield.OnChain.DEX.PartialOrderConfig.Compiled (
    originalPartialOrderConfigValidator,
    optimizedPartialOrderConfigValidator,
    optimizedPartialOrderConfigValidatorWithTracing,
) where

import           Data.Default                               (def)
import           Data.Text                                  (Text)
import           GeniusYield.OnChain.DEX.PartialOrderConfig
import           GeniusYield.OnChain.Plutarch.Api           (PAssetClass)
import           GeniusYield.OnChain.Utils                  (desiredTracingMode)
import           GeniusYield.Plutonomy                      ()
import           Plutarch
import qualified Plutarch.Api.V2                            as PV2
import qualified Plutarch.Unsafe                            as PUNSAFE
import qualified Plutonomy
import           PlutusLedgerApi.V1.Value                   (AssetClass)
import           Ply                                        hiding ((#))
import           Ply.Plutarch

type POConfigScript = TypedScript 'ValidatorRole '[AssetClass]

originalPartialOrderConfigValidator :: Config -> Either Text POConfigScript
originalPartialOrderConfigValidator cnf = toTypedScript cnf mkPartialOrderConfigValidator'

optimizedPartialOrderConfigValidator :: Either Text POConfigScript
optimizedPartialOrderConfigValidator = Plutonomy.optimizeUPLC <$> originalPartialOrderConfigValidator def

optimizedPartialOrderConfigValidatorWithTracing :: Either Text POConfigScript
optimizedPartialOrderConfigValidatorWithTracing = Plutonomy.optimizeUPLC <$> originalPartialOrderConfigValidator def {tracingMode = desiredTracingMode}

mkPartialOrderConfigValidator' ::
  ClosedTerm ( PAssetClass
          :--> PV2.PValidator
             )
mkPartialOrderConfigValidator' = plam $ \nftAC datm redm ctx ->
  popaque $ mkPartialOrderConfigValidator
    # nftAC
    # PUNSAFE.punsafeCoerce datm
    # PUNSAFE.punsafeCoerce redm
    # ctx
