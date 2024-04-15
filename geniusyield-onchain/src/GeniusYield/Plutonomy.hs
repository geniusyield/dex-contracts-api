{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Extras to be added to @plutonomy@.
module GeniusYield.Plutonomy (
    plutonomyMintingPolicyFromScript,
    plutonomyValidatorFromScript
) where

import qualified Plutonomy
import qualified PlutusTx.Code
import qualified UntypedPlutusCore        as UPLC
import Ply
import qualified Ply.Core.Unsafe as PlyUnsafe

instance Plutonomy.HasUPLC (TypedScript rl params) where
    uplc f ts = PlyUnsafe.unsafeTypedScript ver <$> Plutonomy.uplc f scrpt
      where
        (# ver, scrpt #) = PlyUnsafe.unsafeUnTypedScript ts

renameUPLC :: (name -> name') -> UPLC.Term name uni fun ann -> UPLC.Term name' uni fun ann
renameUPLC rnm = go where
    go (UPLC.Var ann n       ) = UPLC.Var ann (rnm n)
    go (UPLC.LamAbs ann n t  ) = UPLC.LamAbs ann (rnm n) (go t)
    go (UPLC.Apply ann t1 t2 ) = UPLC.Apply ann (go t1) (go t2)
    go (UPLC.Delay ann t     ) = UPLC.Delay ann (go t)
    go (UPLC.Force ann t     ) = UPLC.Force ann (go t)
    go (UPLC.Constant ann con) = UPLC.Constant ann con
    go (UPLC.Builtin ann bn  ) = UPLC.Builtin ann bn
    go (UPLC.Error ann       ) = UPLC.Error ann

renameProgram :: (name -> name') -> UPLC.Program name uni fun ann -> UPLC.Program name' uni fun ann
renameProgram f (UPLC.Program ann ver t) = UPLC.Program ann ver (renameUPLC f t)

namedFromDeBruijn :: UPLC.DeBruijn -> UPLC.NamedDeBruijn
namedFromDeBruijn (UPLC.DeBruijn i) = UPLC.NamedDeBruijn "x" i

plutonomyMintingPolicyFromScript :: TypedScript 'MintingPolicyRole '[] -> Plutonomy.MintingPolicy
plutonomyMintingPolicyFromScript (TypedScript _ s) =
    Plutonomy.mkMintingPolicyScript $
    PlutusTx.Code.DeserializedCode (renameProgram namedFromDeBruijn s) Nothing mempty

plutonomyValidatorFromScript :: TypedScript 'ValidatorRole '[] -> Plutonomy.Validator
plutonomyValidatorFromScript (TypedScript _ s) =
    Plutonomy.mkValidatorScript $
    PlutusTx.Code.DeserializedCode (renameProgram namedFromDeBruijn s) Nothing mempty
