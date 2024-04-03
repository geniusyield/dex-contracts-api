module GeniusYield.Server.Tx (
  TxAPI,
  handleTxApi,
  handleTxSign,
  handleTxSignAndSubmit,
  handleTxSubmit,
) where

import Data.Strict qualified as Strict
import Fmt
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils
import GeniusYield.Types
import RIO hiding (logDebug, logInfo)
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
  logInfo ctx $ "Signing transaction: " +| txToHex tx |+ ""
  case ctxSigningKey of
    Just sk → pure $ signGYTx' tx [somePaymentSigningKeyToSomeSigningKey $ Strict.fst sk]
    Nothing → throwIO $ err500 {errBody = "No signing key configured."}

handleTxSignAndSubmit ∷ Ctx → GYTx → IO GYTxId
handleTxSignAndSubmit ctx tx = do
  logInfo ctx $ "Signing and submitting transaction: " +| txToHex tx |+ ""
  signedTx ← handleTxSign ctx tx
  handleTxSubmit ctx signedTx

handleTxSubmit ∷ Ctx → GYTx → IO GYTxId
handleTxSubmit ctx@Ctx {..} tx = do
  logInfo ctx $ "Submitting transaction: " +| txToHex tx |+ ""
  gySubmitTx ctxProviders tx
