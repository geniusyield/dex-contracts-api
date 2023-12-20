{- |
Module      : GeniusYield.Api.Dex.Types
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Api.Dex.Types (
  HasDexScripts,
  GYDexApiQueryMonad,
  GYDexApiMonad,
) where

import Control.Monad.Reader (MonadReader)
import GeniusYield.Scripts.Dex (HasPartialOrderConfigAddr, HasPartialOrderNftScript, HasPartialOrderScript)
import GeniusYield.TxBuilder.Class (GYTxMonad, GYTxQueryMonad)

type HasDexScripts a = (HasPartialOrderConfigAddr a, HasPartialOrderScript a, HasPartialOrderNftScript a)

type GYDexApiQueryMonad m a = (HasDexScripts a, MonadReader a m, GYTxQueryMonad m)

type GYDexApiMonad m a = (GYDexApiQueryMonad m a, GYTxMonad m)
