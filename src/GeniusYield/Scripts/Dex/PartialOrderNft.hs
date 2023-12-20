{- |
Module      : GeniusYield.Scripts.Dex.PartialOrderNft
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Scripts.Dex.PartialOrderNft (
  -- * Typeclass
  HasPartialOrderNftScript (..),

  -- * Policy
  partialOrderNftMintingPolicy,
) where

import GeniusYield.Scripts.Common
import GeniusYield.Scripts.Dex.PartialOrder (HasPartialOrderScript, partialOrderValidator)
import GeniusYield.Types (
  GYAddress,
  GYAssetClass,
  GYMintingPolicy,
  PlutusVersion (PlutusV2),
  addressToPlutus,
  assetClassToPlutus,
  scriptPlutusHash,
  validatorToScript,
 )
import PlutusLedgerApi.V1 (Address, ScriptHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply (ScriptRole (..), TypedScript, (#))

class HasPartialOrderNftScript a where
  getPartialOrderNftPolicy ∷ a → TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass]

partialOrderNftMintingPolicy
  ∷ (HasPartialOrderNftScript a, HasPartialOrderScript a)
  ⇒ a
  → GYAddress
  → GYAssetClass
  → GYMintingPolicy 'PlutusV2
partialOrderNftMintingPolicy a addr ac =
  mintingPolicyFromPly
    $ getPartialOrderNftPolicy a
    # scriptPlutusHash (validatorToScript v)
    # addressToPlutus addr
    # assetClassToPlutus ac
 where
  v = partialOrderValidator a addr ac
