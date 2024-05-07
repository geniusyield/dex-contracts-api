module GeniusYield.Server.Ctx (
  DEXInfo (..),
  dexInfoDefaultMainnet,
  dexInfoDefaultPreprod,
  TapToolsApiKey,
  TapToolsEnv (..),
  Ctx (..),
  runSkeletonI,
  runSkeletonWithStrategyI,
  runSkeletonF,
  runSkeletonWithStrategyF,
  runQuery,
  runQueryWithReader,
) where

import Data.Strict.Tuple (Pair (..))
import GeniusYield.Imports
import GeniusYield.OrderBot.Adapter.Maestro (MaestroProvider)
import GeniusYield.Server.Constants (DEXInfo (..), dexInfoDefaultMainnet, dexInfoDefaultPreprod)
import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types
import RIO
import Servant.Client (ClientEnv)

type TapToolsApiKey = Text

data TapToolsEnv = TapToolsEnv
  { tteClientEnv ∷ !ClientEnv,
    tteApiKey ∷ !TapToolsApiKey
  }

-- | Server context: configuration & shared state.
data Ctx = Ctx
  { ctxNetworkId ∷ !GYNetworkId,
    ctxProviders ∷ !GYProviders,
    ctxDexInfo ∷ !DEXInfo,
    ctxMaestroProvider ∷ !MaestroProvider,
    ctxTapToolsProvider ∷ !(Maybe TapToolsEnv),
    ctxSigningKey ∷ !(Maybe (Pair GYSomePaymentSigningKey GYAddress)),
    ctxCollateral ∷ !(Maybe GYTxOutRef),
    ctxStakeAddress ∷ !(Maybe GYStakeAddressBech32)
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
  let nid = ctxNetworkId ctx
      providers = ctxProviders ctx
      di = ctxDexInfo ctx
      mcollateral' = do
        collateral ← mcollateral
        pure (collateral, False)

  runGYTxMonadNodeF cstrat nid providers (addr : addrs) addr mcollateral' $ runReaderT skeleton di

runQuery ∷ Ctx → ReaderT DEXInfo GYTxQueryMonadNode a → IO a
runQuery ctx = runQueryWithReader ctx (ctxDexInfo ctx)

runQueryWithReader ∷ Ctx → a → ReaderT a GYTxQueryMonadNode b → IO b
runQueryWithReader ctx a q = do
  let nid = ctxNetworkId ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadNode nid providers $ runReaderT q a
