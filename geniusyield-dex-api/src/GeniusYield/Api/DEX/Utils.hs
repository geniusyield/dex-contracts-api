{- |
Module      : GeniusYield.Api.DEX.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Api.DEX.Utils (
  NftInfo (..),
  nftInfo',
  nftInfo,
  stampPlaced,
  stampFilled,
  stampCancel,
) where

import Control.Monad.Reader (ask)
import GeniusYield.Api.Types
import GeniusYield.Scripts
import GeniusYield.Scripts.DEX
import GeniusYield.TxBuilder
import GeniusYield.Types

data NftInfo = NftInfo
  { nftPolicy :: !(GYScript PlutusV2),
    nftName :: !GYTokenName,
    nftToken :: !GYAssetClass,
    nftRef :: !GYTxOutRef,
    nftRedeemer :: !GYRedeemer
  }

nftInfo' :: GYApiQueryMonad m => GYTxOutRef -> m NftInfo
nftInfo' ref = do
  gycs <- ask
  let tn = expectedTokenName ref
  return
    NftInfo
      { nftPolicy = dexNftPolicy gycs,
        nftName = tn,
        nftToken = GYToken (nftMintingPolicyId gycs) tn,
        nftRef = ref,
        nftRedeemer = mkNftRedeemer $ Just ref
      }

nftInfo :: GYApiMonad m => m NftInfo
nftInfo = someUTxOWithoutRefScript >>= nftInfo'

-- | Metadata stamps
stampPlaced, stampFilled, stampCancel :: Maybe GYTxMetadata
stampPlaced = metadataMsg "GeniusYield: Order placed"
stampFilled = metadataMsg "GeniusYield: Order filled"
stampCancel = metadataMsg "GeniusYield: Order canceled"
