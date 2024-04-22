module GeniusYield.OnChain.DEX.NFT.Compiled (
    originalNftPolicy,
    optimizedNftPolicy
) where


import Data.Text (Text)
import Data.Default (def)

import qualified Plutarch.Unsafe as PUNSAFE
import Plutarch.Prelude
import Ply hiding ((#))
import Ply.Plutarch
import qualified Plutarch.Api.V2 as PV2
import qualified Plutonomy

import           GeniusYield.OnChain.DEX.NFT
import           GeniusYield.Plutonomy ()

originalNftPolicy :: Either Text (TypedScript 'MintingPolicyRole '[])
originalNftPolicy = toTypedScript def mkNFTPolicy'

optimizedNftPolicy :: Either Text (TypedScript 'MintingPolicyRole '[])
optimizedNftPolicy = Plutonomy.optimizeUPLC <$> originalNftPolicy

mkNFTPolicy' :: ClosedTerm PV2.PMintingPolicy
mkNFTPolicy' = plam $ \redm ctx ->
  popaque $ mkNFTPolicy
    # PUNSAFE.punsafeCoerce redm
    # ctx

