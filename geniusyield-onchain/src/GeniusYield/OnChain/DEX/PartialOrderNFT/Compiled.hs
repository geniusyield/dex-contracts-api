module GeniusYield.OnChain.DEX.PartialOrderNFT.Compiled (
    originalPartialOrderNftPolicy,
    optimizedPartialOrderNftPolicy,
    optimizedPartialOrderNftPolicyWithTracing
) where


import           Data.Default                            (def)
import           Data.Text                               (Text)
import           Plutarch                                (Config (tracingMode))
import           Plutarch.Api.V1                         (PAddress)
import           Plutarch.Api.V1.Scripts                 (PScriptHash)
import qualified Plutarch.Api.V2                         as PV2
import           Plutarch.Prelude                        (ClosedTerm, plam,
                                                          popaque, type (:-->),
                                                          (#))
import qualified Plutarch.Unsafe                         as PUNSAFE
import qualified Plutonomy
import           PlutusLedgerApi.V1                      (Address, ScriptHash)
import           PlutusLedgerApi.V1.Value                (AssetClass)
import           Ply                                     (ScriptRole (MintingPolicyRole),
                                                          TypedScript)
import           Ply.Plutarch                            (toTypedScript)

import           GeniusYield.OnChain.DEX.PartialOrderNFT (mkPartialOrderNFTPolicy)
import           GeniusYield.OnChain.Plutarch.Types      (PAssetClass)
import           GeniusYield.OnChain.Utils               (desiredTracingMode)
import           GeniusYield.Plutonomy                   ()

originalPartialOrderNftPolicy :: Config
                              -> Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
originalPartialOrderNftPolicy cnf = toTypedScript cnf mkPartialOrderNFTPolicy'

optimizedPartialOrderNftPolicy :: Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
optimizedPartialOrderNftPolicy = Plutonomy.optimizeUPLC <$> originalPartialOrderNftPolicy def

optimizedPartialOrderNftPolicyWithTracing :: Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
optimizedPartialOrderNftPolicyWithTracing = Plutonomy.optimizeUPLC <$> originalPartialOrderNftPolicy def {tracingMode = desiredTracingMode}

mkPartialOrderNFTPolicy' :: ClosedTerm (PScriptHash :--> PAddress :--> PAssetClass :--> PV2.PMintingPolicy)
mkPartialOrderNFTPolicy' = plam $ \sh refInputAddr refInputToken redm ctx ->
  popaque $ mkPartialOrderNFTPolicy
    # sh
    # refInputAddr
    # refInputToken
    # PUNSAFE.punsafeCoerce redm
    # ctx

