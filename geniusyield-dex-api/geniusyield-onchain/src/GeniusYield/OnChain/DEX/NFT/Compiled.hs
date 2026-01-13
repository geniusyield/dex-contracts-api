module GeniusYield.OnChain.DEX.NFT.Compiled (
  originalNftPolicy,
  optimizedNftPolicy,
) where

import Data.Default (def)
import Data.Text (Text)
import GeniusYield.OnChain.DEX.NFT
import GeniusYield.Plutonomy ()
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Prelude
import Plutarch.Unsafe qualified as PUNSAFE
import Plutonomy qualified
import Ply hiding ((#))
import Ply.Plutarch

originalNftPolicy :: Either Text (TypedScript 'MintingPolicyRole '[])
originalNftPolicy = toTypedScript def mkNFTPolicy'

optimizedNftPolicy :: Either Text (TypedScript 'MintingPolicyRole '[])
optimizedNftPolicy = Plutonomy.optimizeUPLC <$> originalNftPolicy

mkNFTPolicy' :: ClosedTerm PV2.PMintingPolicy
mkNFTPolicy' = plam $ \redm ctx ->
  popaque $
    mkNFTPolicy
      # PUNSAFE.punsafeCoerce redm
      # ctx
