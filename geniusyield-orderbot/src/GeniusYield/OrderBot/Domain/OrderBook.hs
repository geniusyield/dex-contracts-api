{- |
Module      : GeniusYield.OrderBot.Domain.OrderBook
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}

-- TODO: Give haddock to module like it's description.
module GeniusYield.OrderBot.Domain.OrderBook
  ( HasOrderBook ( .. )

  ) where
import           GeniusYield.OrderBot.Types (OrderAssetPair, OrderType)

-- TODO: Give haddock.
-- TODO: Use strict versions like strict pair, maybe, list.
class HasOrderBook a where
  type OrderBook a = r | r -> a
  type MultiAssetOrderBook a = r | r -> a
  type Orders (a :: OrderType)
  populateOrderBook :: [OrderAssetPair] -> IO (MultiAssetOrderBook a)
