module GeniusYield.OrderBot.Domain.Markets (
  HasMarkets (..),
) where

import GeniusYield.OrderBot.Types (OrderAssetPair)
import RIO

-- TODO: Change from List?
class HasMarkets a where
  getMarkets ∷ a → IO [OrderAssetPair]
