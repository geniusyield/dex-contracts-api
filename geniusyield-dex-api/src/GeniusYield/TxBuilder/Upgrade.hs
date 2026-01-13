{-# LANGUAGE LambdaCase #-}

module GeniusYield.TxBuilder.Upgrade (
  upgradeTxSkeleton,
  upgradeTxSkeletonToV3,
) where

import Data.Map.Strict qualified as Map
import GeniusYield.TxBuilder.Common (
  GYTxSkeleton (..),
  GYTxSkeletonProposalProcedures (..),
  GYTxSkeletonRefIns (..),
  GYTxSkeletonVotingProcedures (..),
 )
import GeniusYield.Types
import GeniusYield.Types.TxCert.Internal (GYTxCert (..))
import Unsafe.Coerce (unsafeCoerce)

upgradeTxSkeleton :: GYTxSkeleton PlutusV2 -> GYTxSkeleton PlutusV3
upgradeTxSkeleton = upgradeTxSkeletonToV3

upgradeTxSkeletonToV3 :: GYTxSkeleton PlutusV2 -> GYTxSkeleton PlutusV3
upgradeTxSkeletonToV3 GYTxSkeleton {..} =
  GYTxSkeleton
    { gytxIns = fmap upgradeTxIn gytxIns,
      gytxOuts = fmap upgradeTxOut gytxOuts,
      gytxRefIns = upgradeRefIns gytxRefIns,
      gytxMint = Map.fromList $ fmap upgradeMintEntry (Map.toList gytxMint),
      gytxWdrls = fmap upgradeTxWdrl gytxWdrls,
      gytxSigs = gytxSigs,
      gytxCerts = fmap upgradeTxCert gytxCerts,
      gytxInvalidBefore = gytxInvalidBefore,
      gytxInvalidAfter = gytxInvalidAfter,
      gytxMetadata = gytxMetadata,
      gytxVotingProcedures = upgradeVotingProcedures gytxVotingProcedures,
      gytxProposalProcedures = upgradeProposalProcedures gytxProposalProcedures
    }
 where
  upgradeMintEntry
    :: (GYBuildScript PlutusV2, (Map.Map GYTokenName Integer, GYRedeemer))
    -> (GYBuildScript PlutusV3, (Map.Map GYTokenName Integer, GYRedeemer))
  upgradeMintEntry (script, payload) = (upgradeBuildScript script, payload)

upgradeTxIn :: GYTxIn PlutusV2 -> GYTxIn PlutusV3
upgradeTxIn GYTxIn {..} =
  GYTxIn
    { gyTxInTxOutRef = gyTxInTxOutRef,
      gyTxInWitness = upgradeTxInWitness gyTxInWitness
    }

upgradeTxInWitness :: GYTxInWitness PlutusV2 -> GYTxInWitness PlutusV3
upgradeTxInWitness = \case
  GYTxInWitnessKey -> GYTxInWitnessKey
  GYTxInWitnessScript script md redeemer ->
    GYTxInWitnessScript (upgradeBuildPlutusScript script) md redeemer
  GYTxInWitnessSimpleScript script ->
    GYTxInWitnessSimpleScript (upgradeBuildSimpleScript script)

upgradeTxOut :: GYTxOut PlutusV2 -> GYTxOut PlutusV3
upgradeTxOut GYTxOut {..} =
  GYTxOut
    { gyTxOutAddress = gyTxOutAddress,
      gyTxOutValue = gyTxOutValue,
      gyTxOutDatum = fmap (fmap upgradeTxOutUseInlineDatum) gyTxOutDatum,
      gyTxOutRefS = gyTxOutRefS
    }

upgradeTxOutUseInlineDatum
  :: GYTxOutUseInlineDatum PlutusV2
  -> GYTxOutUseInlineDatum PlutusV3
upgradeTxOutUseInlineDatum = \case
  GYTxOutUseInlineDatum -> GYTxOutUseInlineDatum
  GYTxOutDontUseInlineDatum -> GYTxOutDontUseInlineDatum

upgradeRefIns
  :: GYTxSkeletonRefIns PlutusV2
  -> GYTxSkeletonRefIns PlutusV3
upgradeRefIns = \case
  GYTxSkeletonNoRefIns -> GYTxSkeletonNoRefIns
  GYTxSkeletonRefIns refs -> GYTxSkeletonRefIns refs

upgradeTxWdrl :: GYTxWdrl PlutusV2 -> GYTxWdrl PlutusV3
upgradeTxWdrl GYTxWdrl {..} =
  GYTxWdrl
    { gyTxWdrlStakeAddress = gyTxWdrlStakeAddress,
      gyTxWdrlAmount = gyTxWdrlAmount,
      gyTxWdrlWitness = upgradeBuildWitness gyTxWdrlWitness
    }

upgradeTxCert :: GYTxCert PlutusV2 -> GYTxCert PlutusV3
upgradeTxCert GYTxCert {..} =
  GYTxCert
    { gyTxCertCertificate = gyTxCertCertificate,
      gyTxCertWitness = fmap upgradeBuildWitness gyTxCertWitness
    }

upgradeVotingProcedures
  :: GYTxSkeletonVotingProcedures PlutusV2
  -> GYTxSkeletonVotingProcedures PlutusV3
upgradeVotingProcedures = \case
  GYTxSkeletonVotingProceduresNone -> GYTxSkeletonVotingProceduresNone

upgradeProposalProcedures
  :: GYTxSkeletonProposalProcedures PlutusV2
  -> GYTxSkeletonProposalProcedures PlutusV3
upgradeProposalProcedures = \case
  GYTxSkeletonProposalProceduresNone -> GYTxSkeletonProposalProceduresNone

upgradeBuildWitness
  :: GYTxBuildWitness PlutusV2
  -> GYTxBuildWitness PlutusV3
upgradeBuildWitness = \case
  GYTxBuildWitnessKey -> GYTxBuildWitnessKey
  GYTxBuildWitnessPlutusScript script redeemer ->
    GYTxBuildWitnessPlutusScript (upgradeBuildPlutusScript script) redeemer
  GYTxBuildWitnessSimpleScript script ->
    GYTxBuildWitnessSimpleScript (upgradeBuildSimpleScript script)

upgradeBuildScript
  :: GYBuildScript PlutusV2
  -> GYBuildScript PlutusV3
upgradeBuildScript = \case
  GYBuildPlutusScript script ->
    GYBuildPlutusScript (upgradeBuildPlutusScript script)
  GYBuildSimpleScript script ->
    GYBuildSimpleScript (upgradeBuildSimpleScript script)

upgradeBuildPlutusScript
  :: GYBuildPlutusScript PlutusV2
  -> GYBuildPlutusScript PlutusV3
upgradeBuildPlutusScript = unsafeCoerce

upgradeBuildSimpleScript
  :: GYBuildSimpleScript PlutusV2
  -> GYBuildSimpleScript PlutusV3
upgradeBuildSimpleScript = unsafeCoerce
