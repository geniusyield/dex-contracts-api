module GeniusYield.Server.Tx (
  TxAPI,
  handleTxApi,
) where

import GeniusYield.Server.Ctx
import GeniusYield.Types
import Servant

type TxAPI =
  "sign"
    :> Summary "Sign a transaction"
    :> Description "Signs the given transaction using key configured in server."
    :> ReqBody '[JSON] GYTx
    :> Post '[JSON] GYTx
    :<|> "sign-and-submit"
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
