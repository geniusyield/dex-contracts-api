module GeniusYield.Scripts.DEX.NFT (
  nftMintingPolicy,
  mkNftRedeemer,

  -- * shared functions
  expectedTokenName,
  expectedTwoTokenName,
  expectedTwoTokenNameN,
) where

import Data.Maybe (fromJust)
import GeniusYield.OnChain.Core.Common.Utils qualified as OnChain
import GeniusYield.Scripts.Internal (
  GYCompiledScriptsRaw (GYCompiledScriptsRaw, gycsDEXNFTPolicy),
  mintingPolicyFromPly,
 )
import GeniusYield.Types

nftMintingPolicy :: GYCompiledScriptsRaw -> GYScript PlutusV2
nftMintingPolicy GYCompiledScriptsRaw {gycsDEXNFTPolicy} =
  mintingPolicyFromPly gycsDEXNFTPolicy

mkNftRedeemer :: Maybe GYTxOutRef -> GYRedeemer
mkNftRedeemer = redeemerFromPlutusData . fmap txOutRefToPlutus

expectedTokenName :: GYTxOutRef -> GYTokenName
expectedTokenName = fromJust . tokenNameFromPlutus . OnChain.expectedTokenName . txOutRefToPlutus

expectedTwoTokenName :: GYTxOutRef -> GYTokenName
expectedTwoTokenName = fromJust . tokenNameFromPlutus . OnChain.expectedTwoTokenName . txOutRefToPlutus

expectedTwoTokenNameN :: GYTxOutRef -> Integer -> GYTokenName
expectedTwoTokenNameN ref n = fromJust . tokenNameFromPlutus $ OnChain.expectedTwoTokenNameN (txOutRefToPlutus ref) n
