module GeniusYield.Server.Ctx where

-- TODO: Add explicit export list.

import Control.Monad.Reader (ReaderT (..))
import GeniusYield.Api.Dex.PartialOrder (PORefs (..))
import GeniusYield.GYConfig
import GeniusYield.Imports
import GeniusYield.OrderBot.Adapter.Maestro (MaestroProvider)
import GeniusYield.Scripts (HasPartialOrderConfigAddr (..), HasPartialOrderNftScript (..), HasPartialOrderScript (..))
import GeniusYield.Server.Constants (poConfigAddrMainnet, poConfigAddrPreprod, poRefsMainnet, poRefsPreprod)
import GeniusYield.Server.Files (nftPolicy, orderValidator)
import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1 (Address)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply (ScriptRole (..), TypedScript)

-- | Type that encapsulates the scripts needed for the dex api.
data DEXInfo = DEXInfo
  { dexPartialOrderValidator ∷ !(TypedScript 'ValidatorRole '[Address, AssetClass]),
    dexNftPolicy ∷ !(TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass]),
    dexPartialOrderConfigAddr ∷ !GYAddress,
    dexPORefs ∷ !PORefs
  }
  deriving stock (Show)

instance HasPartialOrderScript DEXInfo where
  getPartialOrderValidator = dexPartialOrderValidator

instance HasPartialOrderNftScript DEXInfo where
  getPartialOrderNftPolicy = dexNftPolicy

instance HasPartialOrderConfigAddr DEXInfo where
  getPartialOrderConfigAddr = dexPartialOrderConfigAddr

dexInfoDefaultMainnet ∷ DEXInfo
dexInfoDefaultMainnet =
  DEXInfo
    { dexPartialOrderValidator = orderValidator,
      dexNftPolicy = nftPolicy,
      dexPartialOrderConfigAddr = poConfigAddrMainnet,
      dexPORefs = poRefsMainnet
    }

dexInfoDefaultPreprod ∷ DEXInfo
dexInfoDefaultPreprod =
  DEXInfo
    { dexPartialOrderValidator = orderValidator,
      dexNftPolicy = nftPolicy,
      dexPartialOrderConfigAddr = poConfigAddrPreprod,
      dexPORefs = poRefsPreprod
    }

-- | Server context: configuration & shared state.
data Ctx = Ctx
  { ctxGYCoreConfig ∷ !GYCoreConfig,
    ctxProviders ∷ !GYProviders,
    ctxDexInfo ∷ !DEXInfo,
    ctxMaestroProvider ∷ !MaestroProvider
  }

-- | Create 'TxBody' from a 'GYTxSkeleton'.
runSkeletonI
  ∷ Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT DEXInfo GYTxMonadNode (GYTxSkeleton v)
  → IO GYTxBody
runSkeletonI = coerce (runSkeletonF @Identity)

-- | Create 'TxBody' from a 'GYTxSkeleton', with the specified coin selection strategy.
runSkeletonWithStrategyI
  ∷ GYCoinSelectionStrategy
  → Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT DEXInfo GYTxMonadNode (GYTxSkeleton v)
  → IO GYTxBody
runSkeletonWithStrategyI cstrat = coerce (runSkeletonWithStrategyF @Identity cstrat)

runSkeletonF
  ∷ Traversable t
  ⇒ Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT DEXInfo GYTxMonadNode (t (GYTxSkeleton v))
  → IO (t GYTxBody)
runSkeletonF = runSkeletonWithStrategyF GYRandomImproveMultiAsset

runSkeletonWithStrategyF
  ∷ Traversable t
  ⇒ GYCoinSelectionStrategy
  → Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT DEXInfo GYTxMonadNode (t (GYTxSkeleton v))
  → IO (t GYTxBody)
runSkeletonWithStrategyF cstrat ctx addrs addr mcollateral skeleton = do
  let nid = cfgNetworkId $ ctxGYCoreConfig ctx
      providers = ctxProviders ctx
      di = ctxDexInfo ctx
      mcollateral' = do
        collateral ← mcollateral
        pure (collateral, True)

  runGYTxMonadNodeF cstrat nid providers (addr : addrs) addr mcollateral' $ runReaderT skeleton di

runQuery ∷ Ctx → ReaderT DEXInfo GYTxQueryMonadNode a → IO a
runQuery ctx = runQueryWithReader ctx (ctxDexInfo ctx)

runQueryWithReader ∷ Ctx → a → ReaderT a GYTxQueryMonadNode b → IO b
runQueryWithReader ctx a q = do
  let nid = cfgNetworkId $ ctxGYCoreConfig ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadNode nid providers $ runReaderT q a
