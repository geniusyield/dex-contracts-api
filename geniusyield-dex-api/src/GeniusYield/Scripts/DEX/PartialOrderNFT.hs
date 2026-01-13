module GeniusYield.Scripts.DEX.PartialOrderNFT (
  partialOrderNftMintingPolicy,
) where

import GeniusYield.Scripts.DEX.PartialOrder (partialOrderValidator)
import GeniusYield.Scripts.DEX.PartialOrderConfig (POCVersion (..), partialOrderConfigPlutusAddr)
import GeniusYield.Scripts.Internal (
  GYCompiledScriptsRaw (GYCompiledScriptsRaw, gycsDEXPartialOrderNFTPolicy, gycsDEXPartialOrderNFTV1_1Policy),
  mintingPolicyFromPly,
 )
import GeniusYield.Types (
  GYAssetClass,
  GYScript,
  PlutusVersion (PlutusV2),
  assetClassToPlutus,
  scriptPlutusHash,
  validatorToScript,
 )
import Ply ((#))

partialOrderNftMintingPolicy
  :: GYCompiledScriptsRaw
  -> POCVersion
  -> GYAssetClass
  -> GYScript PlutusV2
partialOrderNftMintingPolicy gycs@GYCompiledScriptsRaw {gycsDEXPartialOrderNFTPolicy, gycsDEXPartialOrderNFTV1_1Policy} pocVersion ac =
  mintingPolicyFromPly $
    (case pocVersion of POCVersion1 -> gycsDEXPartialOrderNFTPolicy; POCVersion1_1 -> gycsDEXPartialOrderNFTV1_1Policy)
      # scriptPlutusHash (validatorToScript v)
      # addr
      # assetClassToPlutus ac
 where
  v = partialOrderValidator gycs pocVersion ac
  addr = partialOrderConfigPlutusAddr gycs pocVersion ac
