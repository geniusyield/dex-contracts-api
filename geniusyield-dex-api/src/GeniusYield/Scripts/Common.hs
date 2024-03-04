{- |
Module      : GeniusYield.Scripts.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Scripts.Common (
  validatorFromPly,
  mintingPolicyFromPly,
) where

import GeniusYield.Types.PlutusVersion
import GeniusYield.Types.Script
import PlutusLedgerApi.V1 (serialiseUPLC)
import Ply (
  ScriptRole (..),
  TypedScript (..),
 )
import Ply qualified

validatorFromPly ∷ ∀ v. SingPlutusVersionI v ⇒ TypedScript 'ValidatorRole '[] → GYValidator v
validatorFromPly ts = case ver' of
  SingPlutusV1 →
    if ver == Ply.ScriptV1
      then validatorFromSerialisedScript @'PlutusV1 $ toSerialisedValidator ts
      else error "validatorFromPly: Invalid script version"
  SingPlutusV2 →
    if ver == Ply.ScriptV2
      then validatorFromSerialisedScript @'PlutusV2 $ toSerialisedValidator ts
      else error "validatorFromPly: Invalid script version"
 where
  ver = Ply.getPlutusVersion ts
  ver' = singPlutusVersion @v
  toSerialisedValidator (TypedScript _ s) = serialiseUPLC s

mintingPolicyFromPly ∷ ∀ v. SingPlutusVersionI v ⇒ TypedScript 'MintingPolicyRole '[] → GYMintingPolicy v
mintingPolicyFromPly ts = case ver' of
  SingPlutusV1 →
    if ver == Ply.ScriptV1
      then mintingPolicyFromSerialisedScript @'PlutusV1 $ toSerialisedMintingPolicy ts
      else error "mintingPolicyFromPly: Invalid script version"
  SingPlutusV2 →
    if ver == Ply.ScriptV2
      then mintingPolicyFromSerialisedScript @'PlutusV2 $ toSerialisedMintingPolicy ts
      else error "mintingPolicyFromPly: Invalid script version"
 where
  ver = Ply.getPlutusVersion ts
  ver' = singPlutusVersion @v
  toSerialisedMintingPolicy (TypedScript _ s) = serialiseUPLC s
