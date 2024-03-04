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
import GeniusYield.Scripts.Dex.PartialOrderConfig (HasPartialOrderConfigAddr (getPartialOrderConfigAddr))
import GeniusYield.Types (
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
  ∷ (HasPartialOrderNftScript a, HasPartialOrderScript a, HasPartialOrderConfigAddr a)
  ⇒ a
  → GYAssetClass
  → GYMintingPolicy 'PlutusV2
partialOrderNftMintingPolicy a ac =
  mintingPolicyFromPly $
    getPartialOrderNftPolicy a
      # scriptPlutusHash (validatorToScript v)
      # addressToPlutus (getPartialOrderConfigAddr a)
      # assetClassToPlutus ac
 where
  v = partialOrderValidator a ac
