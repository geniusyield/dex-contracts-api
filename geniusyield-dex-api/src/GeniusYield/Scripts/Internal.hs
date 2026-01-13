module GeniusYield.Scripts.Internal (
  GYCompiledScriptsRaw (..),
  validatorFromPly,
  mintingPolicyFromPly,
  readCompiledScriptsRaw,
) where

import GeniusYield.OnChain.Core.Common.Scripts
import GeniusYield.Types.PlutusVersion
import GeniusYield.Types.Script
import Paths_geniusyield_dex_api (getDataFileName)
import PlutusLedgerApi.V1 (
  Address,
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  ScriptHash,
  TokenName,
  serialiseUPLC,
 )
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx.Prelude qualified as PlutusTx
import Ply (ScriptRole (..), TypedScript (..), readTypedScript)
import Ply qualified

-- | Record of all the compiled scripts loaded from the filesystem.
data GYCompiledScriptsRaw = GYCompiledScriptsRaw
  { gycsTokenSaleOrder
      :: !( TypedScript
              'ValidatorRole
              '[ POSIXTime,
                 POSIXTime,
                 CurrencySymbol,
                 TokenName,
                 PlutusTx.Rational,
                 PubKeyHash,
                 Integer,
                 PlutusTx.Rational,
                 Address,
                 TokenName
               ]
          ),
    gycsSalePhaseToken
      :: !( TypedScript
              'MintingPolicyRole
              '[ TokenName,
                 POSIXTime,
                 POSIXTime,
                 Address
               ]
          ),
    gycsDEXPartialOrder :: !(TypedScript 'ValidatorRole '[Address, AssetClass]),
    gycsDEXPartialOrderNFTPolicy :: !(TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass]),
    gycsDEXPartialOrderNFTV1_1Policy :: !(TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass]),
    gycsDEXPartialOrderConfig :: !(TypedScript 'ValidatorRole '[AssetClass]),
    gycsDEXPartialOrderConfigV1AppliedPreprod :: !(GYScript 'PlutusV2),
    gycsDEXPartialOrderConfigV1AppliedMainnet :: !(GYScript 'PlutusV2),
    gycsDEXNFTPolicy :: !(TypedScript 'MintingPolicyRole '[]),
    gycsDEXTwoWayOrderConfig :: !(TypedScript 'ValidatorRole '[AssetClass]),
    gycsStakingStake :: !(TypedScript 'ValidatorRole '[]),
    gycsSignedMint :: !(TypedScript 'MintingPolicyRole '[PubKeyHash])
  }

validatorFromPly :: forall v. SingPlutusVersionI v => TypedScript 'ValidatorRole '[] -> GYScript v
validatorFromPly ts = case ver' of
  SingPlutusV1 ->
    if ver == Ply.ScriptV1
      then validatorFromSerialisedScript @PlutusV1 $ toSerialisedValidator ts
      else error "validatorFromPly: Invalid script version"
  SingPlutusV2 ->
    if ver == Ply.ScriptV2
      then validatorFromSerialisedScript @PlutusV2 $ toSerialisedValidator ts
      else error "validatorFromPly: Invalid script version"
  SingPlutusV3 -> error "validatorFromPly: Plutus V3 not supported"
 where
  ver = Ply.getPlutusVersion ts
  ver' = singPlutusVersion @v
  toSerialisedValidator (TypedScript _ s) = serialiseUPLC s

mintingPolicyFromPly :: forall v. SingPlutusVersionI v => TypedScript 'MintingPolicyRole '[] -> GYScript v
mintingPolicyFromPly ts = case ver' of
  SingPlutusV1 ->
    if ver == Ply.ScriptV1
      then mintingPolicyFromSerialisedScript @PlutusV1 $ toSerialisedMintingPolicy ts
      else error "mintingPolicyFromPly: Invalid script version"
  SingPlutusV2 ->
    if ver == Ply.ScriptV2
      then mintingPolicyFromSerialisedScript @PlutusV2 $ toSerialisedMintingPolicy ts
      else error "mintingPolicyFromPly: Invalid script version"
  SingPlutusV3 -> error "mintingPolicyFromPly: Plutus V3 not supported"
 where
  ver = Ply.getPlutusVersion ts
  ver' = singPlutusVersion @v
  toSerialisedMintingPolicy (TypedScript _ s) = serialiseUPLC s

{- | Given a path to the directory containing all the ply compiled GY scripts
(as named in "GeniusYield.OnChain.Core.Common.Scripts"), read all the scripts to build a 'GYCompiledScripts'.
-}
readCompiledScriptsRaw :: IO GYCompiledScriptsRaw
readCompiledScriptsRaw = do
  dexNft <- getDataFileName dex'NFTFile >>= readTypedScript
  dexPartialOrder <- getDataFileName dex'PartialOrderFile >>= readTypedScript
  dexPartialOrderNft <- getDataFileName dex'PartialOrderNFTFile >>= readTypedScript
  dexPartialOrderNftV1_1 <- getDataFileName dex'PartialOrderNFTV1_1File >>= readTypedScript
  dexPartialOrderConfig <- getDataFileName dex'PartialOrderConfigFile >>= readTypedScript
  dexTwoWayOrderConfig <- getDataFileName dex'TwoWayOrderConfigFile >>= readTypedScript
  stakingStake <- getDataFileName staking'OldStakeFile >>= readTypedScript
  tokenSaleOrder <- getDataFileName tokenSale'OrderFile >>= readTypedScript
  tokenSaleToken <- getDataFileName tokenSale'SalePhaseTokenFile >>= readTypedScript
  dexPartialOrderConfigV1AppliedPreprod <- getDataFileName "DEX.PartialOrderConfigV1AppliedPreprod" >>= readValidator
  dexPartialOrderConfigV1AppliedMainnet <- getDataFileName "DEX.PartialOrderConfigV1AppliedMainnet" >>= readValidator
  signedMint <- getDataFileName signedMintFile >>= readTypedScript
  pure $
    GYCompiledScriptsRaw
      { gycsTokenSaleOrder = tokenSaleOrder,
        gycsSalePhaseToken = tokenSaleToken,
        gycsDEXPartialOrder = dexPartialOrder,
        gycsDEXPartialOrderNFTPolicy = dexPartialOrderNft,
        gycsDEXPartialOrderNFTV1_1Policy = dexPartialOrderNftV1_1,
        gycsDEXPartialOrderConfig = dexPartialOrderConfig,
        gycsDEXPartialOrderConfigV1AppliedPreprod = dexPartialOrderConfigV1AppliedPreprod,
        gycsDEXPartialOrderConfigV1AppliedMainnet = dexPartialOrderConfigV1AppliedMainnet,
        gycsDEXNFTPolicy = dexNft,
        gycsDEXTwoWayOrderConfig = dexTwoWayOrderConfig,
        gycsStakingStake = stakingStake,
        gycsSignedMint = signedMint
      }
