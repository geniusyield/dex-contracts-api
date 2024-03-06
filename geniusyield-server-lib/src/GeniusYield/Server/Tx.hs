module GeniusYield.Server.Tx (
  TxAPI,
  handleTxApi,
) where

import Control.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as T
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Api.Dex.PartialOrder (PORefs (..), PartialOrderInfo (..), cancelMultiplePartialOrders, getPartialOrdersInfos, partialOrders, placePartialOrder')
import GeniusYield.Api.Dex.PartialOrderConfig (fetchPartialOrderConfig)
import GeniusYield.HTTP.Errors (
  GYApiError (..),
  IsGYApiError (..),
 )
import GeniusYield.Imports
import GeniusYield.OrderBot.Types (OrderAssetPair (..), mkEquivalentAssetPair, mkOrderAssetPair)
import GeniusYield.Scripts.Dex.PartialOrderConfig (PartialOrderConfigInfoF (..))
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils (addSwaggerDescription, dropAndCamelToSnake, logInfo, unsignedTxHex)
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import Network.HTTP.Types (status400)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import RIO.Map qualified as Map
import Servant

type TransactionSignPrefix ∷ Symbol
type TransactionSignPrefix = "tf"

-- TODO: JSON & Swagger instances.
data TransactionSign = TransactionSign
  { tsTransaction ∷ !GYTx
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TransactionSignPrefix, CamelToSnake]] TransactionSign

instance Swagger.ToSchema TransactionSign where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropAndCamelToSnake @TransactionSignPrefix}

type TxAPI =
  "sign"
    :> Summary "Sign a transaction"
    :> Description "Signs the given transaction using key configured in server."
    :> ReqBody '[JSON] GYTx
    :> Post '[JSON] GYTx
    :<|> "sign_and_submit"
      :> Summary "Sign and submit a transaction"
      :> Description "Signs the given transaction using key configured in server and submits it to the network."
      :> ReqBody '[JSON] GYTx
      :> Post '[JSON] GYTxId
    :<|> "submit"
      :> Summary "Submit a transaction"
      :> Description "Submits the given transaction to the network."
      :> ReqBody '[JSON] GYTx
      :> Post '[JSON] GYTxId

handleTxApi ∷ Ctx → ServerT TxAPI IO
handleTxApi ctx =
  handleTxSign ctx
    :<|> handleTxSignAndSubmit ctx
    :<|> handleTxSubmit ctx

handleTxSign ∷ Ctx → GYTx → IO GYTx
handleTxSign ctx@Ctx {..} tx = do
  -- TODO: Add log.
  -- TODO: add signature.
  pure tx

handleTxSignAndSubmit ∷ Ctx → GYTx → IO GYTxId
handleTxSignAndSubmit ctx@Ctx {..} tx = do
  -- TODO: Add log.
  -- TODO: add logic.
  pure $ "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"

handleTxSubmit ∷ Ctx → GYTx → IO GYTxId
handleTxSubmit ctx@Ctx {..} tx = do
  -- TODO: Add log.
  -- TODO: add logic.
  pure $ "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"
