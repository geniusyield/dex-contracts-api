module GeniusYield.Api.DEX.PartialOrderConfig (
  PORef (..),
  SomePORef (..),
  withSomePORef,
  PORefs (..),
  PocdException (..),
  partialOrderConfigAddr,
  -- , deployPartialOrderConfig
  fetchPartialOrderConfig,
  unsafeFetchPartialOrderConfig,
  fetchPartialOrderConfig',
  unsafeFetchPartialOrderConfig',
  RefPocd (..),
  SomeRefPocd (..),
  withSomeRefPocd,
  RefPocds,
  selectV1RefPocd,
  selectV1_1RefPocd,
  selectRefPocd,
  selectRefPocd',
  selectPor,
  selectPor',
  fetchPartialOrderConfigs,
  updatePartialOrderConfig,
) where

import Control.Monad.Reader (ask)
import Data.Strict.Tuple (Pair (..))
import Data.Text qualified as Txt
-- , mustMint

import GeniusYield.Api.Types
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports
import GeniusYield.Scripts (GYCompiledScripts (..))
import GeniusYield.Scripts.DEX.PartialOrderConfig (
  POCVersion (..),
  PartialOrderConfigDatumF (..),
  SingPOCVersion (..),
  SingPOCVersionI (singPOCVersion),
  fromSingPOCVersion,
  toSingPOCVersion,
  withSomeSingPOCVersion,
 )
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

data PORef (v :: POCVersion) = PORef
  { -- | The reference NFT.
    porRefNft :: !GYAssetClass,
    -- | The location of the reference NFT minting policy reference script.
    porMintRef :: !GYTxOutRef,
    -- | The location of the validator reference script.
    porValRef :: !GYTxOutRef
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SomePORef = forall v. SingPOCVersionI v => SomePORef (PORef v)

withSomePORef :: SomePORef -> (forall v. SingPOCVersionI v => PORef v -> r) -> r
withSomePORef (SomePORef por) f = f por

data PORefs = PORefs
  { -- | For the V1 version of partial order family of contract.
    porV1 :: !(PORef 'POCVersion1),
    -- | For the V1_1 version of partial order family of contract.
    porV1_1 :: !(PORef 'POCVersion1_1)
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype PocdException = PocdException GYAssetClass
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError PocdException where
  toApiError (PocdException nftToken) =
    GYApiError
      { gaeErrorCode = "PARTIAL_ORDER_CONFIG_NOT_FOUND",
        gaeHttpStatus = status400,
        gaeMsg = Txt.pack $ printf "Partial order config not found for NFT: %s" nftToken
      }

partialOrderConfigAddr :: GYApiQueryMonad m => POCVersion -> GYAssetClass -> m GYAddress
partialOrderConfigAddr pocVersion nftToken = do
  gycs <- ask
  scriptAddress $ dexPartialOrderConfigValidator gycs pocVersion nftToken

{-
deployPartialOrderConfig
  :: GYApiMonad m
  => POCVersion
  -> [GYPubKeyHash]
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
  -> Natural
  -- ^ Minimum required deposit (in lovelace).
  -> m (GYAssetClass, GYTxSkeleton PlutusV2)
deployPartialOrderConfig pocVersion signatories reqSignatories feeAddr makerFeeFlat makerFeeRatio takerFee minDeposit = do
  gycs <- ask
  NftInfo {..} <- nftInfo
  addr <- partialOrderConfigAddr pocVersion nftToken
  let
    policy = dexPartialOrderNftPolicy gycs pocVersion nftToken
    mintingScript = mintingPolicyToScript policy
    validatorScript = validatorToScript $ dexPartialOrderValidator gycs pocVersion nftToken
    datum =
      PartialOrderConfigDatum
        { pocdSignatories = signatories
        , pocdReqSignatories = max 1 $ toInteger reqSignatories
        , pocdNftSymbol = mintingPolicyId policy
        , pocdFeeAddr = feeAddr
        , pocdMakerFeeFlat = toInteger makerFeeFlat
        , pocdMakerFeeRatio = max 0 makerFeeRatio
        , pocdTakerFee = toInteger takerFee
        , pocdMinDeposit = toInteger minDeposit
        }
    skeleton =
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
        <> deployScript validatorScript

  pure (nftToken, skeleton)
-}

newtype RefPocd (v :: POCVersion) = RefPocd (Pair GYTxOutRef (PartialOrderConfigDatumF GYAddress))

data SomeRefPocd = forall v. SingPOCVersionI v => SomeRefPocd (RefPocd v)

withSomeRefPocd :: SomeRefPocd -> (forall v. SingPOCVersionI v => RefPocd v -> r) -> r
withSomeRefPocd (SomeRefPocd por) f = f por

newtype RefPocds = RefPocds (Pair (RefPocd 'POCVersion1) (RefPocd 'POCVersion1_1))

selectV1RefPocd :: RefPocds -> RefPocd 'POCVersion1
selectV1RefPocd (RefPocds (p :!: _)) = p

selectV1_1RefPocd :: RefPocds -> RefPocd 'POCVersion1_1
selectV1_1RefPocd (RefPocds (_ :!: p)) = p

selectRefPocd :: RefPocds -> POCVersion -> SomeRefPocd
selectRefPocd refPocds pocVersion = withSomeSingPOCVersion (toSingPOCVersion pocVersion) (\(_ :: SingPOCVersion v) -> SomeRefPocd (selectRefPocd' @v refPocds))

selectRefPocd' :: forall v. SingPOCVersionI v => RefPocds -> RefPocd v
selectRefPocd' refPocds = case (singPOCVersion @v) of
  SingPOCVersion1 -> selectV1RefPocd refPocds
  SingPOCVersion1_1 -> selectV1_1RefPocd refPocds

selectPor :: PORefs -> POCVersion -> SomePORef
selectPor pors pocVersion = withSomeSingPOCVersion (toSingPOCVersion pocVersion) (\(_ :: SingPOCVersion v) -> SomePORef (selectPor' @v pors))

selectPor' :: forall v. SingPOCVersionI v => PORefs -> PORef v
selectPor' PORefs {..} = case (singPOCVersion @v) of
  SingPOCVersion1 -> porV1
  SingPOCVersion1_1 -> porV1_1

fetchPartialOrderConfig :: GYApiQueryMonad m => POCVersion -> PORefs -> m SomeRefPocd
fetchPartialOrderConfig pocVersion pors =
  let SomePORef (PORef {..}) = selectPor pors pocVersion
   in unsafeFetchPartialOrderConfig pocVersion porRefNft

-- | Unsafe as it takes NFT's asset class where this NFT might not belong to the given version.
unsafeFetchPartialOrderConfig :: GYApiQueryMonad m => POCVersion -> GYAssetClass -> m SomeRefPocd
unsafeFetchPartialOrderConfig pocVersion nftToken =
  withSomeSingPOCVersion (toSingPOCVersion pocVersion) $ \(_ :: SingPOCVersion v) -> SomeRefPocd <$> unsafeFetchPartialOrderConfig' @v nftToken

-- | Unsafe as it takes NFT's asset class where this NFT might not belong to the given version.
unsafeFetchPartialOrderConfig' :: forall v m. (GYApiQueryMonad m, SingPOCVersionI v) => GYAssetClass -> m (RefPocd v)
unsafeFetchPartialOrderConfig' nftToken = do
  let pocVersion = fromSingPOCVersion $ singPOCVersion @v
  addr <- partialOrderConfigAddr pocVersion nftToken
  utxos <- utxosAtAddressWithDatums addr $ Just nftToken
  case utxos of
    [p@(utxo, Just _)] -> do
      (_, _, d') <- utxoDatumPure' p
      feeAddr <- addressFromPlutus' $ pocdFeeAddr d'
      pure $ RefPocd $ utxoRef utxo :!: feeAddr <$ d'
    _ -> throwAppError $ PocdException nftToken

fetchPartialOrderConfig' :: forall v m. (GYApiQueryMonad m, SingPOCVersionI v) => PORefs -> m (RefPocd v)
fetchPartialOrderConfig' pors = do
  let
    pocVersion = fromSingPOCVersion $ singPOCVersion @v
    SomePORef (PORef {..}) = selectPor pors pocVersion
  unsafeFetchPartialOrderConfig' @v porRefNft

fetchPartialOrderConfigs :: GYApiQueryMonad m => PORefs -> m RefPocds
fetchPartialOrderConfigs pors = do
  refPocd1 <- fetchPartialOrderConfig' @'POCVersion1 pors
  refPocd1_1 <- fetchPartialOrderConfig' @'POCVersion1_1 pors
  pure $ RefPocds $ refPocd1 :!: refPocd1_1

updatePartialOrderConfig
  :: GYApiMonad m
  => POCVersion
  -> [GYPubKeyHash]
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
  -> Natural
  -- ^ Minimum required deposit (in lovelace).
  -> GYAssetClass
  -- ^ The reference NFT.
  -> [GYPubKeyHash]
  -- ^ Expected signatures.
  -> m (GYTxSkeleton PlutusV2)
updatePartialOrderConfig
  pocVersion
  signatories
  reqSignatories
  feeAddr
  makerFeeFlat
  makerFeeRatio
  takerFee
  minDeposit
  nftToken
  expSignatures = do
    SomeRefPocd (RefPocd (ref :!: pocd)) <- unsafeFetchPartialOrderConfig pocVersion nftToken
    addr <- partialOrderConfigAddr pocVersion nftToken
    gycs <- ask

    let
      validator = dexPartialOrderConfigValidator gycs pocVersion nftToken
      datum =
        pocd
          { pocdSignatories = signatories,
            pocdReqSignatories = max 1 $ toInteger reqSignatories,
            pocdFeeAddr = feeAddr,
            pocdMakerFeeFlat = toInteger makerFeeFlat,
            pocdMakerFeeRatio = max 0 makerFeeRatio,
            pocdTakerFee = toInteger takerFee,
            pocdMinDeposit = toInteger minDeposit
          }

    pure $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = ref,
            gyTxInWitness =
              GYTxInWitnessScript
                (GYInScript validator)
                (Just $ datumFromPlutusData pocd)
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
        <> if pocVersion == POCVersion1
          then mempty
          else
            mustHaveOutput
              GYTxOut
                { gyTxOutAddress = feeAddr,
                  gyTxOutValue = mempty,
                  gyTxOutDatum = Nothing,
                  gyTxOutRefS = Nothing
                }
