module GeniusYield.Api.DEX.Exceptions (
  FillOrderException (..),
  validFillRangeConstraints,
) where

import Control.Exception
import Data.Text qualified as Text
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types
import Network.HTTP.Types.Status

-- | Exceptions raised while (partially) filling (partial) orders.
data FillOrderException
  = -- | Attempt to (partially) fill an order too early.
    TooEarlyFill {foeStart :: !GYSlot, foeNow :: !GYSlot}
  | -- | Attempt to (partially) fill an order too late.
    TooLateFill {foeEnd :: !GYSlot, foeNow :: !GYSlot}
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError FillOrderException where
  toApiError (TooEarlyFill start now) =
    GYApiError
      { gaeErrorCode = "TOO_EARLY_FILL",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ printf "Order cannot be filled before slot %s\ncurrent slot: %s" start now
      }
  toApiError (TooLateFill end now) =
    GYApiError
      { gaeErrorCode = "TOO_LATE_FILL",
        gaeHttpStatus = status400,
        gaeMsg = Text.pack $ printf "Order cannot be filled after slot %s\ncurrent slot: %s" end now
      }

validFillRangeConstraints
  :: forall m (v :: PlutusVersion)
   . GYTxQueryMonad m
  => Maybe GYTime
  -> Maybe GYTime
  -> m (GYTxSkeleton v)
validFillRangeConstraints mstart mend = (<>) <$> startConstraint <*> endConstraint
 where
  startConstraint :: m (GYTxSkeleton v)
  startConstraint = case mstart of
    Nothing -> return mempty
    Just start -> do
      now <- slotOfCurrentBlock
      startSlot <- enclosingSlotFromTime' start
      if now >= startSlot
        then return $ isInvalidBefore now
        else throwAppError $ TooEarlyFill {foeStart = startSlot, foeNow = now}

  endConstraint :: m (GYTxSkeleton v)
  endConstraint = case mend of
    Nothing -> return mempty
    Just end -> do
      now <- slotOfCurrentBlock
      endSlot <- enclosingSlotFromTime' end
      if now <= endSlot
        then return $ isInvalidAfter $ min endSlot $ unsafeAdvanceSlot now 120
        else throwAppError $ TooLateFill {foeEnd = endSlot, foeNow = now}
