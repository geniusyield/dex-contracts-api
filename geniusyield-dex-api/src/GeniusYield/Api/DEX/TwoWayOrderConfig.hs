module GeniusYield.Api.DEX.TwoWayOrderConfig (
  TWORef (..),
  RefTWOCD (..),
  TWOSkeleton (..),
  -- , deployTwoWayOrderConfig
  -- , deployTwoWayOrderConfigPlan
  fetchTwoWayOrderConfig,
  updateTwoWayOrderConfig,
  twoWayOrderConfigAddr,
) where

import Control.Monad.Reader (ask)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Strict.Tuple (Pair (..))
import Data.Text qualified as Txt
-- , mustHaveCertificate

-- , mustHaveRefInput
-- , mustMint

import GeniusYield.Api.Types
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports
import GeniusYield.Scripts (GYCompiledScripts (..))
import GeniusYield.Scripts.DEX.TwoWayOrderConfig
import GeniusYield.TxBuilder (
  GYTxQueryMonad (utxosAtAddressWithDatums),
  GYTxSkeleton,
  addressFromPlutus',
  mustBeSignedBy,
  mustHaveInput,
  mustHaveOutput,
  scriptAddress,
  throwAppError,
  utxoDatumPure',
 )
import GeniusYield.Types
import Network.HTTP.Types (status400)

newtype TwocdException = TwocdException GYAssetClass
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError TwocdException where
  toApiError (TwocdException nftToken) =
    GYApiError
      { gaeErrorCode = "TWO_WAY_ORDER_CONFIG_NOT_FOUND",
        gaeHttpStatus = status400,
        gaeMsg =
          Txt.pack $
            printf "Two-way order config not found for NFT: %s" nftToken
      }

twoWayOrderConfigAddr :: GYApiQueryMonad m => GYAssetClass -> m GYAddress
twoWayOrderConfigAddr nftToken = do
  gycs <- ask
  scriptAddress $ dexTwoWayOrderConfigValidator gycs nftToken

data TWOSkeleton tx = MkTWOSkeleton GYAssetClass (NonEmpty tx) tx
  deriving stock (Foldable, Functor, Traversable)

{-
deployTwoWayOrderConfig
  :: GYApiMonad m
  => [GYPubKeyHash]
  -- ^ Public key hashes of the potential signatories.
  -> Natural
  -- ^ Number of required signatures.
  -> GYAddress
  -- ^ Address to which fees are paid.
  -> Natural
  -- ^ Flat fee (in lovelace) paid by the maker.
  -> GYRational
  -- ^ Proportional fee (in the offered token) paid by the maker.
  -> Natural
  -- ^ Flat fee (in lovelace) paid by the taker.
  -> GYRational
  -- ^ Proportional fee (in the offered token) paid by the taker.
  -> Natural
  -- ^ Maximum age (in seconds) for oracle prices accepted by fills.
  -> Natural
  -- ^ Minimum required deposit (in lovelace).
  -> m (TWOSkeleton (GYTxSkeleton PlutusV2))
deployTwoWayOrderConfig
  signatories
  reqSignatories
  feeAddr
  makerFeeFlat
  makerFeeRatio
  takerFeeFlat
  takerFeeRatio
  oracleFreshnessSeconds
  minDeposit = do
    NftInfo {..} <- nftInfo
    addr <- twoWayOrderConfigAddr nftToken

    let
      mintingScript = twoWayOrderMintValidator nftToken
      spendScript = twoWayOrderSpendValidator nftToken

      fillScript = twoWayOrderFillValidator nftToken
      cancelScript = twoWayOrderCancelValidator nftToken

      datum =
        TwoWayOrderConfigDatum
          { twocdSignatories = signatories
          , twocdReqSignatories = max 1 $ toInteger reqSignatories
          , twocdNftSymbol = mintingPolicyId mintingScript
          , twocdFillHash = scriptHash fillScript
          , twocdCancelHash = scriptHash cancelScript
          , twocdFeeAddr = feeAddr
          , twocdMakerFeeFlat = toInteger makerFeeFlat
          , twocdMakerFeeRatio = max 0 makerFeeRatio -- ???: why do we check it off-chain
          , twocdTakerFeeFlat = toInteger takerFeeFlat
          , twocdTakerFeeRatio = max 0 takerFeeRatio -- ???: why do we check it off-chain
          , twocdOracleFreshnessSeconds = toInteger oracleFreshnessSeconds
          , twocdMinDeposit = toInteger minDeposit
          }

      spendScriptRef = deployScript spendScript
      fillScriptRef = deployScript fillScript
      cancelScriptRef = deployScript cancelScript

      refsScripts = spendScriptRef :| [fillScriptRef, cancelScriptRef]

      nft'mint =
        mustHaveInput
          GYTxIn
            { gyTxInTxOutRef = nftRef
            , gyTxInWitness = GYTxInWitnessKey
            }
          <> mustMint (GYMintScript nftPolicy) nftRedeemer nftName 1
          <> mustHaveOutput
            GYTxOut
              { gyTxOutAddress = addr
              , gyTxOutValue = valueSingleton nftToken 1
              , gyTxOutDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum)
              , gyTxOutRefS = Nothing
              }
          <> deployScript mintingScript
    pure $ MkTWOSkeleton nftToken refsScripts nft'mint
-}

{- | Build a deployment plan that encodes the correct submission order
(mint first, then reference scripts) and provides a helper to derive
references from the resulting transaction ids.

deployTwoWayOrderConfigPlan
  :: GYApiMonad m
  => [GYPubKeyHash]
  -> Natural
  -> GYAddress
  -> Natural
  -> GYRational
  -> Natural
  -> GYRational
  -> Natural
  -> Natural
  -> m
       ( GYAssetClass
       , GYTxSkeleton PlutusV2
       , -- \^ mint tx (submit first)
         NonEmpty (GYTxSkeleton PlutusV2)
       , -- \^ reference script txs (submit sequentially)
         (GYTxId, NonEmpty GYTxId) -> (TWORef, [GYTxSkeleton PlutusV2])
       )
-- \^ derive refs and stake registrations

deployTwoWayOrderConfigPlan sigs req feeAddr makerFeeFlat makerFeeRatio takerFeeFlat takerFeeRatio oracleFreshnessSeconds minDeposit = do
  MkTWOSkeleton refNft refsTxs mintTx <-
    deployTwoWayOrderConfig sigs req feeAddr makerFeeFlat makerFeeRatio takerFeeFlat takerFeeRatio oracleFreshnessSeconds minDeposit
  let
    fillWithdrawScript = twoWayOrderFillValidator refNft
    fillPublishScript = twoWayOrderFillPublishValidator refNft
    cancelWithdrawScript = twoWayOrderCancelValidator refNft
    cancelPublishScript = twoWayOrderCancelPublishValidator refNft
    fillHash = scriptHash fillWithdrawScript
    cancelHash = scriptHash cancelWithdrawScript

    derive (mintTid, refsTids) =
      let
        mintRef = txOutRefFromTuple (mintTid, 1)
        (spendTid :| restTids) = refsTids
        (fillTid, cancelTid) = case restTids of
          [ft, ct] -> (ft, ct)
          _ -> error "deployTwoWayOrderConfigPlan: unexpected reference tx id list"
        spendRef = txOutRefFromTuple (spendTid, 0)
        fillRef = txOutRefFromTuple (fillTid, 0)
        cancelRef = txOutRefFromTuple (cancelTid, 0)

        fillStakeCred = GYStakeCredentialByScript fillHash
        cancelStakeCred = GYStakeCredentialByScript cancelHash

        fillWitness =
          GYTxBuildWitnessPlutusScript
            (GYBuildPlutusScriptReference fillRef fillPublishScript)
            unitRedeemer
        cancelWitness =
          GYTxBuildWitnessPlutusScript
            (GYBuildPlutusScriptReference cancelRef cancelPublishScript)
            unitRedeemer

        registerFill =
          mustHaveRefInput fillRef
            <> mustHaveCertificate (mkStakeAddressRegistrationCertificate fillStakeCred fillWitness)
        registerCancel =
          mustHaveRefInput cancelRef
            <> mustHaveCertificate (mkStakeAddressRegistrationCertificate cancelStakeCred cancelWitness)
      in
        ( TWORef
            { tworRefNft = refNft
            , tworMintRef = mintRef
            , tworSpendRef = spendRef
            , tworFillRef = fillRef
            , tworCancelRef = cancelRef
            }
        , [registerFill, registerCancel]
        )
  pure (refNft, mintTx, refsTxs, derive)
-}
data TWORef = TWORef
  { -- | The reference NFT.
    tworRefNft :: !GYAssetClass,
    -- | The location of the reference NFT minting policy reference script.
    tworMintRef :: !GYTxOutRef,
    -- | The location of the validator reference script.
    tworSpendRef :: !GYTxOutRef,
    -- | Reference script UTxO for the fill stake validator.
    tworFillRef :: !GYTxOutRef,
    -- | Reference script UTxO for the cancel stake validator.
    tworCancelRef :: !GYTxOutRef
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype RefTWOCD = RefTWOCD (Pair GYTxOutRef (TwoWayOrderConfigDatumF GYAddress))

fetchTwoWayOrderConfig
  :: GYApiQueryMonad m
  => GYAssetClass -> m RefTWOCD
fetchTwoWayOrderConfig nftToken = do
  addr <- twoWayOrderConfigAddr nftToken
  utxos <- utxosAtAddressWithDatums addr $ Just nftToken
  case utxos of
    [p@(utxo, Just _)] -> do
      (_, _, d') <- utxoDatumPure' p
      feeAddr <- addressFromPlutus' $ twocdFeeAddr d'
      pure . RefTWOCD $ utxoRef utxo :!: feeAddr <$ d'
    _ -> throwAppError $ TwocdException nftToken

updateTwoWayOrderConfig
  :: GYApiMonad m
  => [GYPubKeyHash]
  -- ^ Public key hashes of the potential signatories.
  -> Natural
  -- ^ Number of required signatures.
  -> GYAddress
  -- ^ Address to which fees are paid.
  -> Natural
  -- ^ Flat fee (in lovelace) paid by the maker.
  -> GYRational
  -- ^ Proportional fee (in the offered token) paid by the maker.
  -> Natural
  -- ^ Flat fee (in lovelace) paid by the taker.
  -> GYRational
  -- ^ Proportional fee (in the offered token) paid by the taker.
  -> Natural
  -- ^ Maximum age (in seconds) for oracle prices accepted by fills.
  -> Natural
  -- ^ Minimum required deposit (in lovelace).
  -> GYAssetClass
  -- ^ The reference NFT.
  -> [GYPubKeyHash]
  -- ^ Expected signatures.
  -> m (GYTxSkeleton PlutusV2)
updateTwoWayOrderConfig
  signatories
  reqSignatories
  feeAddr
  makerFeeFlat
  makerFeeRatio
  takerFeeFlat
  takerFeeRatio
  oracleFreshnessSeconds
  minDeposit
  nftToken
  expSignatures = do
    RefTWOCD (ref :!: twocd) <- fetchTwoWayOrderConfig nftToken
    addr <- twoWayOrderConfigAddr nftToken
    gycs <- ask
    let
      validator = dexTwoWayOrderConfigValidator gycs nftToken
      datum =
        twocd
          { twocdSignatories = signatories,
            twocdReqSignatories = max 1 $ toInteger reqSignatories,
            twocdFeeAddr = feeAddr,
            twocdMakerFeeFlat = toInteger makerFeeFlat,
            twocdMakerFeeRatio = max 0 makerFeeRatio, -- ???: why do we check it off-chain
            twocdTakerFeeFlat = toInteger takerFeeFlat,
            twocdTakerFeeRatio = max 0 takerFeeRatio, -- ???: why do we check it off-chain
            twocdOracleFreshnessSeconds = toInteger oracleFreshnessSeconds,
            twocdMinDeposit = toInteger minDeposit
          }
    pure $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = ref,
            gyTxInWitness =
              GYTxInWitnessScript
                (GYInScript validator)
                Nothing
                unitRedeemer
          }
        <> mustHaveOutput
          GYTxOut
            { gyTxOutAddress = addr,
              gyTxOutValue = valueSingleton nftToken 1,
              gyTxOutDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum),
              gyTxOutRefS = Nothing
            }
        <> mconcat (mustBeSignedBy <$> expSignatures)
        <> mustHaveOutput
          GYTxOut
            { gyTxOutAddress = feeAddr,
              gyTxOutValue = mempty,
              gyTxOutDatum = Nothing,
              gyTxOutRefS = Nothing
            }
