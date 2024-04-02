module GeniusYield.OrderBot.Domain.Markets (
  HasMarkets (..),
  OrderAssetPair (currencyAsset, commodityAsset),
  mkOrderAssetPair,
  equivalentAssetPair,
  mkEquivalentAssetPair,
) where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import GeniusYield.Types (GYAssetClass)
import RIO
import RIO.Text qualified as Text
import RIO.Text.Partial qualified as Text
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

{- | The asset pair in a DEX Order.

All 'OrderAssetPair's constructed out of equivalent raw asset pairs, must compare equal. See: 'equivalentAssetPair'.

For each unique asset pair (see: 'mkAssetPair'), one asset is chosen as the "commodity" (being sold), and the other
is chosen as the "currency" - this makes it simpler to perform order matching.
-}
data OrderAssetPair = OAssetPair
  { currencyAsset ∷ !GYAssetClass,
    commodityAsset ∷ !GYAssetClass
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON OrderAssetPair where
  toJSON oap =
    Aeson.String $ toUrlPiece oap

instance FromJSON OrderAssetPair where
  parseJSON = Aeson.withText "OrderAssetPair" $ \t →
    case parseUrlPiece t of
      Left e → fail $ Text.unpack e
      Right oap → pure oap

-- >>> toUrlPiece $ OAssetPair {currencyAsset = GYLovelace, commodityAsset = GYLovelace}
-- "_"
-- >>> toUrlPiece $ OAssetPair {currencyAsset = GYToken "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535" "AGIX", commodityAsset = GYToken "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535" "AGIX"}
-- "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958_f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958"
-- >>> toUrlPiece $ OAssetPair {currencyAsset = GYLovelace, commodityAsset = GYToken "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535" "AGIX"}
-- "_f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958"
instance ToHttpApiData OrderAssetPair where
  toUrlPiece OAssetPair {..} = toUrlPiece currencyAsset <> "_" <> toUrlPiece commodityAsset

-- >>> parseUrlPiece "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958_f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958" :: Either Text OrderAssetPair
-- Right (OAssetPair {currencyAsset = GYToken "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535" "AGIX", commodityAsset = GYToken "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535" "AGIX"})
-- >>> parseUrlPiece "_f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958" :: Either Text OrderAssetPair
-- Right (OAssetPair {currencyAsset = GYLovelace, commodityAsset = GYToken "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535" "AGIX"})
-- >>> parseUrlPiece "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958_" :: Either Text OrderAssetPair
-- Right (OAssetPair {currencyAsset = GYToken "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535" "AGIX", commodityAsset = GYLovelace})
-- >>> parseUrlPiece "_" :: Either Text OrderAssetPair
-- Right (OAssetPair {currencyAsset = GYLovelace, commodityAsset = GYLovelace})
-- >>> parseUrlPiece "" :: Either Text OrderAssetPair
-- Right (OAssetPair {currencyAsset = GYLovelace, commodityAsset = GYLovelace})
instance FromHttpApiData OrderAssetPair where
  parseUrlPiece t = do
    let (cur, com) = (\com' → if Text.null com' then com' else Text.drop 1 com') <$> Text.breakOn "_" t
    curAsset ← parseUrlPiece cur
    comAsset ← parseUrlPiece com
    pure $ OAssetPair curAsset comAsset

instance Swagger.ToParamSchema OrderAssetPair where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString

instance Swagger.ToSchema OrderAssetPair where
  declareNamedSchema p =
    pure $
      Swagger.named "OrderAssetPair" $
        Swagger.paramSchemaToSchema p
          & Swagger.example
          ?~ toJSON ("f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535.41474958_dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb.0014df1047454e53" ∷ String)
            & Swagger.description
          ?~ "Market pair identifier. It's an underscore delimited concatenation of offered and asked asset's \"token detail\". A token detail is given by dot delimited concatenation of policy id and token name."

{- | Two order asset pairs are considered "equivalent" (but not strictly equal, as in 'Eq'),
     if they contain the same 2 assets irrespective of order.
     i.e {currencyAsset = A, commodityAsset = B} and
         {currencyAsset = B, commodityAsset = A} are equivalent.
-}
equivalentAssetPair ∷ OrderAssetPair → OrderAssetPair → Bool
equivalentAssetPair oap oap' = oap == oap' || oap == mkEquivalentAssetPair oap'

mkEquivalentAssetPair ∷ OrderAssetPair → OrderAssetPair
mkEquivalentAssetPair oap =
  OAssetPair
    { commodityAsset = currencyAsset oap,
      currencyAsset = commodityAsset oap
    }

mkOrderAssetPair
  ∷ GYAssetClass
  -- ^ Asset class of the currency asset in the order.
  → GYAssetClass
  -- ^ Asset class of the commodity asset in the order.
  → OrderAssetPair
mkOrderAssetPair curAsset comAsset =
  OAssetPair
    { currencyAsset = curAsset,
      commodityAsset = comAsset
    }

class HasMarkets a where
  getMarkets ∷ a → IO [OrderAssetPair]
