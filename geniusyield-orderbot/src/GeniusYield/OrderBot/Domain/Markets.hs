module GeniusYield.OrderBot.Domain.Markets (
  HasMarkets (..),
) where

import Deriving.Aeson
import GeniusYield.OrderBot.Types (OrderAssetPair)
import RIO (Data, Natural, Word64, toConstr, (>>>))
import RIO.List (find)
import RIO.Text (Text, pack)
import RIO.Time (Day, UTCTime)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- TODO: Change from List?
class HasMarkets a where
  getMarkets ∷ a → IO [OrderAssetPair]
