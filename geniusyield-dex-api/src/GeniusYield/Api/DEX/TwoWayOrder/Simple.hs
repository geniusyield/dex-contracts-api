{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module GeniusYield.Api.DEX.TwoWayOrder.Simple (
  SingleReverseParams (..),
  SingleFixedPlaceParams (..),
  SingleFixedFillParams (..),
  singleFixedPlace,
  singleFixedFill,
  singleCancel,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Strict.Tuple (Pair ((:!:)))
import GeniusYield.Api.DEX.TwoWayOrder (
  TWDirectionSpec (..),
  TWFillDirection,
  TWFillSpec (..),
  TWORef,
  TWPlaceSpec (..),
  TWPriceSpec (..),
  cancelTwoWayOrders,
  fillTwoWayOrders,
  getTwoWayOrderInfo,
  placeTwoWayOrders,
 )
import GeniusYield.Api.DEX.TwoWayOrderConfig (RefTWOCD (..))
import GeniusYield.Api.Oracle (OracleCertificate)
import GeniusYield.Api.Types (GYApiMonad)
import GeniusYield.Imports
import GeniusYield.TxBuilder.Common (GYTxSkeleton)
import GeniusYield.Types

{- | Parameters describing the reverse leg of a fixed-price order.

When omitted, a one-way order will be placed using only the straight side.
When provided, the resulting specification becomes a true two-way order with
both straight and reverse legs defined explicitly.
-}
data SingleReverseParams = SingleReverseParams
  { srpAmount :: !Natural,
    srpAsset :: !GYAssetClass,
    srpPrice :: !GYRational
  }
  deriving stock (Eq, Show)

{- | Minimal inputs required for placing a single fixed-price two-way order.

This intentionally mirrors the subset of configuration that the CLI exposes
and constrains the builder to the fixed-price path so the command layer does
not need to worry about the richer relative/oracle-based flows.
-}
data SingleFixedPlaceParams = SingleFixedPlaceParams
  { sfppOwner :: !GYAddress,
    sfppOfferAsset :: !GYAssetClass,
    sfppOfferAmount :: !Natural,
    sfppAskAsset :: !GYAssetClass,
    sfppStraightPrice :: !GYRational,
    sfppReverse :: !(Maybe SingleReverseParams),
    sfppStart :: !(Maybe GYTime),
    sfppEnd :: !(Maybe GYTime),
    sfppStakeCredential :: !(Maybe GYStakeCredential)
  }
  deriving stock (Eq, Show)

-- | Parameters used when filling a single fixed-price order.
data SingleFixedFillParams = SingleFixedFillParams
  { sffpOrderRef :: !GYTxOutRef,
    sffpDirection :: !TWFillDirection,
    sffpAmount :: !Natural,
    sffpOracleCertificate :: !(Maybe OracleCertificate),
    sffpRecipient :: !GYAddress
  }
  deriving stock (Eq, Show)

{- | Build the transaction skeleton required to place a single fixed-price
two-way order. Under the hood this simply adapts the provided parameters into
the general 'TWPlaceSpec' and reuses the existing multi-order API in
single-order mode.
-}
singleFixedPlace
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> RefTWOCD
  -> SingleFixedPlaceParams
  -> m (GYTxSkeleton PlutusV3)
singleFixedPlace twor refCfg params =
  let
    spec = toPlaceSpec params
    RefTWOCD (cfgRef :!: twocd) = refCfg
   in
    placeTwoWayOrders twor (spec :| []) cfgRef twocd

{- | Build the transaction skeleton necessary to fill a single fixed-price
order.
-}
singleFixedFill
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> RefTWOCD
  -> SingleFixedFillParams
  -> m (GYTxSkeleton PlutusV3)
singleFixedFill twor refCfg params =
  fillTwoWayOrders twor (toFillSpec params :| []) refCfg

{- | Build the transaction skeleton to cancel a single order by fetching its
current information and delegating to the existing multi-order cancel helper.
-}
singleCancel
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> GYTxOutRef
  -> m (GYTxSkeleton PlutusV3)
singleCancel twor orderRef = do
  info <- getTwoWayOrderInfo twor orderRef
  cancelTwoWayOrders twor [info]

-- Internal: convert convenience params into the underlying placement spec.
toPlaceSpec :: SingleFixedPlaceParams -> TWPlaceSpec
toPlaceSpec SingleFixedPlaceParams {..} =
  TWPlaceSpec
    { twpsOwner = sfppOwner,
      twpsDirection = directionSpec,
      twpsPriceSpec = priceSpec,
      twpsStart = sfppStart,
      twpsEnd = sfppEnd,
      twpsAddLov = 0,
      twpsStakeCred = sfppStakeCredential
    }
 where
  directionSpec = case sfppReverse of
    Nothing -> TWOneWay (sfppOfferAmount, sfppOfferAsset) sfppAskAsset 0
    Just SingleReverseParams {srpAmount, srpAsset} ->
      TWTwoway (sfppOfferAmount, sfppOfferAsset) (srpAmount, srpAsset)

  priceSpec = case sfppReverse of
    Nothing -> TWPriceFixed sfppStraightPrice Nothing
    Just SingleReverseParams {srpPrice} ->
      TWPriceFixed sfppStraightPrice (Just srpPrice)

-- Internal: convert convenience params into the underlying fill spec.
toFillSpec :: SingleFixedFillParams -> TWFillSpec
toFillSpec SingleFixedFillParams {..} =
  TWFillSpec
    { twfsOrderRef = sffpOrderRef,
      twfsDirection = sffpDirection,
      twfsAmount = sffpAmount,
      twfsOracleCertificate = sffpOracleCertificate,
      twfsRecipient = sffpRecipient
    }
