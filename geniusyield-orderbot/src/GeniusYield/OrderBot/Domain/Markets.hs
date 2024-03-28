module GeniusYield.OrderBot.Domain.Markets (
  HasMarkets (..),
) where

import GeniusYield.OrderBot.Types (OrderAssetPair)
import RIO

class HasMarkets a where
  getMarkets ∷ a → IO [OrderAssetPair]
