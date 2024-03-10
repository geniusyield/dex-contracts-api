{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GeniusYield.Server.ErrorMiddleware (
  exceptionHandler,
  errorJsonWrapMiddleware,
  errorLoggerMiddleware,
  apiErrorToServerError,
  missingSignatoryCheck,
) where

import Control.Exception (
  SomeException,
  displayException,
  fromException,
 )
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toUpper)
import Data.IORef (
  modifyIORef',
  newIORef,
  readIORef,
 )
import Data.Text (isInfixOf)
import Data.Text qualified as Txt
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Providers.Common (SubmitTxException (SubmitTxException))
import GeniusYield.Transaction (BuildTxException (..))
import GeniusYield.Transaction.Common (BalancingError (..))
import GeniusYield.TxBuilder
import Network.HTTP.Types (
  Status (statusCode, statusMessage),
  mkStatus,
  status400,
  status500,
 )
import Network.Wai qualified as Wai
import RIO.Text qualified as T
import Servant.Server (ServerError (..))

{- | This is used for turning non-json responses into JSON.

Example of responses which are not in JSON: Servant body parse error, url not found error etc.
-}
errorJsonWrapMiddleware ∷ Wai.Middleware
errorJsonWrapMiddleware app req respond = app req $ \res → do
  let (status, headers, body) = Wai.responseToStream res
  if lookup "Content-Type" headers /= Just "application/json" -- Don't overwrite responses which are already json!
    && statusCode status >= 400
    && statusCode status < 600
    then do
      lbs ←
        if statusCode status == 404
          then -- The body in a 404 Servant err is empty for some reason.
            pure . LBS.fromStrict $ BS8.pack "Not Found"
          else sinkStreamingBody body
      respond $ errorResponse status lbs
    else respond res

errorLoggerMiddleware ∷ (LT.Text → IO ()) → Wai.Middleware
errorLoggerMiddleware errorLogger app req respond = app req $ \res → do
  let (status, _headers, body) = Wai.responseToStream res
  when (statusCode status >= 400 && statusCode status < 600) $
    sinkStreamingBody body >>= errorLogger . lazyDecodeUtf8Lenient
  respond res

{- | Reinterpret exceptions raised by the server (mostly contract exceptions) into 'GYApiError's.

Use 'apiErrorToServerError' to construct a server response out of 'GYApiError'.
-}
exceptionHandler ∷ SomeException → GYApiError
exceptionHandler =
  catchesWaiExc
    [ WH $ \case
        BuildTxBalancingError (BalancingErrorInsufficientFunds x) →
          GYApiError
            { gaeErrorCode = "INSUFFICIENT_BALANCE",
              gaeHttpStatus = status400,
              gaeMsg = "Value dip: " <> tShow x
            }
        BuildTxBalancingError BalancingErrorEmptyOwnUTxOs →
          GYApiError
            { gaeErrorCode = "INSUFFICIENT_BALANCE",
              gaeHttpStatus = status400,
              gaeMsg = "No UTxOs available to build transaction from in wallet"
            }
        BuildTxBalancingError (BalancingErrorChangeShortFall a) →
          GYApiError
            { gaeErrorCode = "INSUFFICIENT_BALANCE",
              gaeHttpStatus = status400,
              gaeMsg = "When trying to balance the transaction, our coin balancer felt short by " <> tShow a <> " lovelaces"
            }
        BuildTxCollateralShortFall req given →
          GYApiError
            { -- This won't really happen as the collateral UTxO we choose has >= 5 ada.
              gaeErrorCode = "INSUFFICIENT_BALANCE",
              gaeHttpStatus = status400,
              gaeMsg = "Total lovelaces required as collateral to build for this transaction " <> tShow req <> " but only available " <> tShow given
            }
        BuildTxNoSuitableCollateral →
          GYApiError
            { gaeErrorCode = "NO_SUITABLE_COLLATERAL",
              gaeHttpStatus = status400,
              gaeMsg = "Could not find the suitable UTxO as collateral, wallet must have a UTxO containing more than " <> tShow collateralLovelace <> " lovelaces"
            }
        e → someBackendError $ displayException' e,
      WH $ \case
        SubmitTxException errBody →
          if "BadInputsUTxO" `isInfixOf` errBody -- See https://github.com/input-output-hk/cardano-ledger/blob/de7c29eef6d7eaabf5d704e976f7840a2edce355/eras/babbage/impl/src/Cardano/Ledger/Babbage/Rules/Utxo.hs#L350-L351.
            then
              GYApiError
                { gaeErrorCode = "UTXO_CONSUMED",
                  gaeHttpStatus = status500,
                  gaeMsg = "Input UTxO referred to by transaction has already been consumed, complete submission error text: " <> errBody
                }
            else
              if missingSignatoryCheck errBody
                then
                  GYApiError
                    { gaeErrorCode = "MISSING_SIGNATORY",
                      gaeHttpStatus = status500,
                      gaeMsg = "User's wallet provider couldn't provide all required witnesses. This issue should likely be resolved by trying out provider with multi-address support enabled. Complete submission error text: " <> errBody
                    }
                else
                  GYApiError
                    { gaeErrorCode = "SUBMISSION_FAILURE",
                      gaeHttpStatus = status500,
                      gaeMsg = errBody
                    },
      WH $ \case
        ServerError {..} → GYApiError {gaeErrorCode = "SERVER_ERROR", gaeHttpStatus = mkStatus errHTTPCode (errReasonPhrase & T.pack & T.encodeUtf8), gaeMsg = LTE.decodeUtf8' errBody & either (const mempty) LT.toStrict},
      WH $ \case
        GYConversionException convErr → someBackendError $ tShow convErr
        GYQueryUTxOException txErr → someBackendError $ tShow txErr
        GYNoSuitableCollateralException minAmt addr →
          someBackendError $
            "No suitable collateral of at least "
              <> tShow minAmt
              <> " was found at the address "
              <> tShow addr
        GYSlotOverflowException slot advAmt →
          someBackendError $
            "Slot value "
              <> tShow slot
              <> " overflows when advanced by "
              <> tShow advAmt
        GYTimeUnderflowException sysStart time →
          someBackendError $
            "Timestamp "
              <> tShow time
              <> " is before known system start "
              <> tShow sysStart
        GYQueryDatumException qdErr → someBackendError $ tShow qdErr
        GYDatumMismatch actualDatum scriptWitness → someBackendError $ "Actual datum in UTxO is: " <> tShow actualDatum <> ", but witness has wrong corresponding datum information: " <> tShow scriptWitness
        GYApplicationException e → toApiError e
    ]

sinkStreamingBody ∷ ((Wai.StreamingBody → IO ()) → IO ()) → IO LBS.ByteString
sinkStreamingBody k = do
  ref ← newIORef mempty
  k $ \f → f (\b → modifyIORef' ref (<> b)) (return ())
  B.toLazyByteString <$> readIORef ref

errorResponse ∷ Status → LBS.ByteString → Wai.Response
errorResponse status body =
  Wai.responseLBS
    status
    [("Content-Type", "application/json")]
    $ Aeson.encode
    $ Aeson.object
      [ "errorCode" Aeson..= bsMsgToCode (statusMessage status),
        "message" Aeson..= LTE.decodeLatin1 body
      ]
 where
  -- bsMsgToCode "Not Found" = "NOT_FOUND"
  bsMsgToCode = Txt.map (\case ' ' → '_'; x → toUpper x) . decodeUtf8Lenient

-- | Transform a 'GYApiError' to 'ServerError'.
apiErrorToServerError ∷ GYApiError → ServerError
apiErrorToServerError GYApiError {gaeHttpStatus, gaeErrorCode, gaeMsg} =
  ServerError
    { errHTTPCode = statusCode gaeHttpStatus,
      errReasonPhrase = Txt.unpack . decodeUtf8Lenient $ statusMessage gaeHttpStatus,
      errBody = Aeson.encode $ Aeson.object ["errorCode" .= gaeErrorCode, "message" .= gaeMsg],
      errHeaders = [("Content-Type", "application/json")]
    }

data WaiExceptionHandler = ∀ e. Exception e ⇒ WH (e → GYApiError)

catchesWaiExc ∷ [WaiExceptionHandler] → SomeException → GYApiError
catchesWaiExc handlers e = foldr tryHandler (someBackendError $ displayException' e) handlers
 where
  tryHandler (WH handler) res = maybe res handler $ fromException e

displayException' ∷ Exception e ⇒ e → Text
displayException' = Txt.pack . displayException

tShow ∷ Show a ⇒ a → Text
tShow = Txt.pack . show

missingSignatoryCheck ∷ Text → Bool
missingSignatoryCheck errBody = "MissingVKeyWitnessesUTXOW" `isInfixOf` errBody || "MissingRequiredSigners" `isInfixOf` errBody
