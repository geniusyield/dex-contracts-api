{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GeniusYield.Api.Utils
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Api.Utils (
  stampRewardsClaimed,
  runGYTxMonadNode,
  runGYTxMonadNodeF,
  runGYTxMonadNodeParallel,
  runGYTxMonadNodeParallelWithStrategy,
  runGYApiMonad,
  runGYTxMonadNodeChainingWithStrategy,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GeniusYield.Transaction.CoinSelection (GYCoinSelectionStrategy)
import GeniusYield.TxBuilder (
  GYTxBuilderMonad (buildTxBodyParallelWithStrategy, buildTxBodyWithStrategy),
  GYTxBuilderMonadIO,
  GYTxQueryMonadIO,
  buildTxBody,
  buildTxBodyChainingWithStrategy,
  buildTxBodyParallel,
  runGYTxBuilderMonadIO,
 )
import GeniusYield.TxBuilder.Common (GYTxBuildResult, GYTxSkeleton)
import GeniusYield.TxBuilder.IO.Unsafe (unsafeIOToQueryMonad, unsafeIOToTxBuilderMonad)
import GeniusYield.Types

instance MonadIO GYTxQueryMonadIO where
  liftIO = unsafeIOToQueryMonad

instance MonadIO GYTxBuilderMonadIO where
  liftIO = unsafeIOToTxBuilderMonad

-- | Metadata stamps
stampRewardsClaimed :: Maybe GYTxMetadata
stampRewardsClaimed = metadataMsg "GeniusYield: Rewards claimed"

runGYTxMonadNode :: GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (GYTxSkeleton v) -> IO GYTxBody
runGYTxMonadNode nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBody

runGYTxMonadNodeF :: forall t v. Traversable t => GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (t (GYTxSkeleton v)) -> IO (t GYTxBody)
runGYTxMonadNodeF strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= traverse (buildTxBodyWithStrategy strat)

runGYTxMonadNodeParallel :: GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO [GYTxSkeleton v] -> IO GYTxBuildResult
runGYTxMonadNodeParallel nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBodyParallel

runGYTxMonadNodeParallelWithStrategy :: GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO [GYTxSkeleton v] -> IO GYTxBuildResult
runGYTxMonadNodeParallelWithStrategy strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBodyParallelWithStrategy strat

-- | Build multiple skeletons with chaining semantics (later txs can spend outputs of earlier ones).
runGYTxMonadNodeChainingWithStrategy :: GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO [GYTxSkeleton v] -> IO GYTxBuildResult
runGYTxMonadNodeChainingWithStrategy strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBodyChainingWithStrategy strat

-- | Run an arbitrary GYApiMonad action (no tx building), using the same environment as the tx builders.
runGYApiMonad :: GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO a -> IO a
runGYApiMonad nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral act
