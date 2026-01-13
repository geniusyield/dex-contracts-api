{- | This module re-exports compiled plutus scripts used by GeniusYield.

The @Scripts@ modulespace contains script compilation,
and the @OnChain@ modulespace contains the actual Plutus Code.
(i.e. @OnChain@ is written in "plutus", @Scripts@ is ordinary Haskell;
the dialects is distinct enough to keep them completely separate).
-}
module GeniusYield.Scripts.DEX (
  GYScripts (..),

  -- * Re-exports
  module GeniusYield.Scripts.DEX.NFT,
  -- , module GeniusYield.Scripts.DEX.Option
  module GeniusYield.Scripts.DEX.PartialOrder,
  module GeniusYield.Scripts.DEX.PartialOrderConfig,
  module GeniusYield.Scripts.DEX.PartialOrderNFT,
  module GeniusYield.Scripts.DEX.TwoWayOrderConfig,
  module GeniusYield.Scripts.TestToken,
) where

import GeniusYield.Scripts.DEX.NFT
import GeniusYield.Scripts.DEX.PartialOrder
import GeniusYield.Scripts.DEX.PartialOrderConfig
import GeniusYield.Scripts.DEX.PartialOrderNFT
import GeniusYield.Scripts.DEX.TwoWayOrderConfig
import GeniusYield.Scripts.TestToken
import GeniusYield.Types

{- | Record holding all scripts with quasi-static parameters applied.

This way we can share the scripts.
-}
data GYScripts = GYScripts
  { gyPartialOrderValidator :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2),
    gyPartialOrderNFTPolicy :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2),
    gyPartialOrderNFTPolicyId :: !(POCVersion -> GYAssetClass -> GYMintingPolicyId)
  }
