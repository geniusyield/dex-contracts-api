module GeniusYield.OnChain.DEX.PartialOrderNFT.Compiled (
  originalPartialOrderNftPolicy,
  optimizedPartialOrderNftPolicy,
  optimizedPartialOrderNftPolicyWithTracing,
) where

import Data.Default (def)
import Data.Text (Text)
import GeniusYield.OnChain.Core.Common.Crypto (ScriptHash)
import GeniusYield.OnChain.DEX.PartialOrderNFT (mkPartialOrderNFTPolicy)
import GeniusYield.OnChain.Plutarch.Types (PAssetClass)
import GeniusYield.OnChain.TokenSale.SalePhaseToken (PAddress)
import GeniusYield.OnChain.Utils (desiredTracingMode)
import GeniusYield.Plutonomy ()
import Plutarch (Config (tracingMode))
import Plutarch.Api.V1.Scripts (PScriptHash)
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Prelude (ClosedTerm, plam, popaque, (#), type (:-->))
import Plutarch.Unsafe qualified as PUNSAFE
import Plutonomy qualified
import PlutusLedgerApi.V1 (Address)
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply (ScriptRole (MintingPolicyRole), TypedScript)
import Ply.Plutarch (toTypedScript)

originalPartialOrderNftPolicy
  :: Config
  -> Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
originalPartialOrderNftPolicy cnf = toTypedScript cnf mkPartialOrderNFTPolicy'

optimizedPartialOrderNftPolicy :: Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
optimizedPartialOrderNftPolicy = Plutonomy.optimizeUPLC <$> originalPartialOrderNftPolicy def

optimizedPartialOrderNftPolicyWithTracing :: Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
optimizedPartialOrderNftPolicyWithTracing = Plutonomy.optimizeUPLC <$> originalPartialOrderNftPolicy def {tracingMode = desiredTracingMode}

mkPartialOrderNFTPolicy' :: ClosedTerm (PScriptHash :--> PAddress :--> PAssetClass :--> PV2.PMintingPolicy)
mkPartialOrderNFTPolicy' = plam $ \sh refInputAddr refInputToken redm ctx ->
  popaque $
    mkPartialOrderNFTPolicy
      # sh
      # refInputAddr
      # refInputToken
      # PUNSAFE.punsafeCoerce redm
      # ctx
