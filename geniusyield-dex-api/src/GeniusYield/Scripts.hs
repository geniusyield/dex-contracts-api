module GeniusYield.Scripts (
  GYCompiledScripts (..),
  GYCompiledScriptsRaw,
  mkCompiledScripts,
  mkGYScripts,
  -- , sptPolicyId
  -- , sptSymbol
  -- , spt
  -- , stakeAddress
  nftMintingPolicyId,
  readCompiledScripts,
  validatorFromPly,
  mintingPolicyFromPly,
) where

import GeniusYield.Scripts.DEX (POCVersion)
import GeniusYield.Scripts.DEX qualified as DEX
import GeniusYield.Scripts.Internal
import GeniusYield.Types

-- should we put Aiken scripts here as well???
data GYCompiledScripts = GYCompiledScripts
  { dexPartialOrderValidator :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2),
    dexPartialOrderNftPolicy :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2),
    dexPartialOrderConfigValidator :: !(POCVersion -> GYAssetClass -> GYScript PlutusV2),
    dexNftPolicy :: !(GYScript PlutusV2),
    dexTwoWayOrderConfigValidator :: !(GYAssetClass -> GYScript PlutusV2)
  }

-- | 'GYScripts' constructor
mkGYScripts
  :: GYCompiledScripts
  -> DEX.GYScripts
mkGYScripts
  GYCompiledScripts
    { dexPartialOrderValidator,
      dexPartialOrderNftPolicy
    } =
    DEX.GYScripts
      { gyPartialOrderValidator = dexPartialOrderValidator,
        gyPartialOrderNFTPolicy = dexPartialOrderNftPolicy,
        gyPartialOrderNFTPolicyId = \pocVersion -> mintingPolicyId . dexPartialOrderNftPolicy pocVersion
      }

instance Show GYCompiledScripts where
  showsPrec _ _ = showString "<GYCompiledScripts>"

readCompiledScripts :: IO GYCompiledScripts
readCompiledScripts = mkCompiledScripts <$> readCompiledScriptsRaw

-- | Create a record of simple functions from a record of typed scripts.
mkCompiledScripts :: GYCompiledScriptsRaw -> GYCompiledScripts
mkCompiledScripts gycs =
  GYCompiledScripts
    { dexPartialOrderValidator = DEX.partialOrderValidator gycs,
      dexPartialOrderNftPolicy = DEX.partialOrderNftMintingPolicy gycs,
      dexPartialOrderConfigValidator = DEX.partialOrderConfigValidator gycs,
      dexNftPolicy = DEX.nftMintingPolicy gycs,
      dexTwoWayOrderConfigValidator = DEX.twoWayOrderConfigValidator gycs
    }

nftMintingPolicyId :: GYCompiledScripts -> GYMintingPolicyId
nftMintingPolicyId GYCompiledScripts {dexNftPolicy} = mintingPolicyId dexNftPolicy
