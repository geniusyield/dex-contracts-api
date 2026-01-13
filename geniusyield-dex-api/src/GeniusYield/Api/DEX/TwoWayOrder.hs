{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GeniusYield.Api.DEX.TwoWayOrder (
  AssetDetails (..),
  Offer (..),
  PriceDelta (..),
  TwoWays (..),
  TWORef (..),
  TwoWayOrderInfo (..),
  TWOrder (..),
  TWOIPrice (..),
  -- New multi-order placement types/APIs
  TWPriceSpec (..),
  TWDirectionSpec (..),
  TWPlaceSpec (..),
  twoWayOrderAddr,
  twoWayOrders,
  twoWayOrdersWithTransformerPredicate,
  cancelTwoWayOrders,
  TWFillDirection (..),
  TWFillSpec (..),
  fillTwoWayOrders,
  fillTwoWayAndLegacyPartialOrders,
  getTwoWayOrderInfo,
  placeTwoWayOrders,
  resolveContinuingDeposit,
  OrderAssets (..),
  OrderPrices (..),
  PriceVal (..),
  extractOrderAssets,
  extractOrderPrices,
  RefTWOCD,
  -- ^ for use in smart-order-router
) where

import Control.Applicative (empty)
import Control.Lens ((?~))
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (MonadError (..))
import Data.Aeson qualified as Aeson
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (type Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Strict.Tuple (Pair (..))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text as Txt (Text, pack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import GHC.Generics (type Generic)
import GHC.Stack (HasCallStack)
import GeniusYield.Api.DEX.PartialOrder (PORefs, fillMultiplePartialOrders)
import GeniusYield.Api.DEX.PartialOrderConfig (RefPocds)
import GeniusYield.Api.DEX.TwoWayOrderConfig (RefTWOCD (..), TWORef (..), fetchTwoWayOrderConfig)
import GeniusYield.Api.DEX.Utils (stampCancel, stampFilled, stampPlaced)
import GeniusYield.Api.Oracle (OracleCertificate (..), Price (..))
import GeniusYield.Api.Types
import GeniusYield.Crypto (SignatureOffchain (..))
import GeniusYield.HTTP.Errors
import GeniusYield.Imports (iwither)
import GeniusYield.Scripts.DEX.NFT
import GeniusYield.Scripts.DEX.TwoWayOrder
import GeniusYield.Scripts.DEX.TwoWayOrder.Utils (mkTwoWayOrderDeltaDatumOneWay, mkTwoWayOrderDeltaDatumTwoWay, mkTwoWayOrderFixedDatum, mkTwoWayOrderFixedDatumTwoWay)
import GeniusYield.Scripts.DEX.TwoWayOrderConfig
import GeniusYield.TxBuilder (
  GYConversionError (..),
  GYTxMonadException (..),
  GYTxQueryMonad,
  GYTxSkeleton,
  addressFromPlutus',
  addressToPubKeyHash',
  enclosingSlotFromTime',
  gyLogDebug',
  isInvalidAfter,
  mustBeSignedBy,
  mustHaveInput,
  mustHaveOutput,
  mustHaveRefInput,
  mustHaveTxMetadata,
  mustHaveWithdrawal,
  mustMint,
  networkId,
  pubKeyHashFromPlutus',
  scriptAddress,
  someUTxOWithoutRefScript,
  throwAppError,
  tokenNameFromPlutus',
  utxoAtTxOutRefWithDatum',
  utxoDatumPureWithOriginalDatum',
  utxosAtPaymentCredentialWithDatums,
  utxosDatumsPureWithOriginalDatum,
 )
import GeniusYield.TxBuilder.Upgrade (upgradeTxSkeleton)
import GeniusYield.Types
import Network.HTTP.Types.Status
import PlutusLedgerApi.Data.V1
import PlutusLedgerApi.V1.Tx qualified as V1
import PlutusLedgerApi.V1.Value as Ledger
import PlutusTx qualified as PlutusTx
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Ratio qualified as Tx
import Prelude hiding (reverse)

{-
-- | Exceptions raised in the 'get two way orders' endpoint.
data GetTWOrdersException
    -- | No utxo found for given ref.
    = NoTWOrderForRef GYTxOutRef
    -- | Not a order UTxO (incorrect/missing datum).
    | InvalidOrderUtxo GYUTxO
    deriving stock Show
    deriving anyclass Exception

instance IsGYApiError GetOrderInfoException where
    toApiError (NoOrderForRef ref) = GYApiError
        { gaeErrorCode = "ORDER_NOT_FOUND"
        , gaeHttpStatus = status404
        , gaeMsg = Txt.pack $ "No order found for ref: " ++ show ref
        }
    toApiError (InvalidOrderUtxo utxo) = GYApiError
        { gaeErrorCode = "INVALID_ORDER"
        , gaeHttpStatus = status400
        , gaeMsg = Txt.pack $ "Not a valid order: " ++ show utxo
        }
-}

data TwoWayOrderInfo = TwoWayOrderInfo
  { twoiRef :: !GYTxOutRef,
    -- \*** --
    twoiOwnerCredentials :: ![GYPaymentCredential],
    twoiOwnerAddr :: !GYAddress,
    twoiNFT :: !GYTokenName,
    twoiOffer :: !(TWOIPrice TWOrder),
    twoiStart :: !(Maybe GYTime),
    twoiEnd :: !(Maybe GYTime),
    twoiTakerLovelaceFlatFee :: !Natural,
    twoiTakerFeeRatio :: !GYRational,
    twoiMakerFeeRatio :: !GYRational,
    twoiOracleFreshnessSeconds :: !Natural,
    -- \*** --
    twoiUTxOValue :: !GYValue,
    twoiUTxOAddr :: !GYAddress,
    twoiNFTCS :: !GYMintingPolicyId,
    twoiRawDatum :: !GYDatum
  }
  deriving stock (Generic, Show, Eq)

instance Swagger.ToSchema GYPaymentVerificationKey where
  declareNamedSchema _ =
    pure
      . Swagger.named "GYPaymentVerificationKey"
      $ mempty
        & Swagger.type_
          ?~ Swagger.SwaggerString
        & Swagger.format
          ?~ "hex"
        & Swagger.description
          ?~ "Payment Verification Key."
        & Swagger.example
          ?~ Aeson.toJSON @Text "58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
        & Swagger.maxLength
          ?~ 68
        & Swagger.minLength
          ?~ 68

newtype TWOrder price = TWOrder (TwoWays (Offer price))

deriving newtype instance Show p => Show (TWOrder p)

deriving newtype instance Eq p => Eq (TWOrder p)

deriving newtype instance Generic p => Generic (TWOrder p)

deriving newtype instance (Generic p, Swagger.ToSchema p) => Swagger.ToSchema (TWOrder p)

deriving newtype instance (Aeson.ToJSON p, Generic p) => Aeson.ToJSON (TWOrder p)

-- \| Price that is set in the order.
data TWOIPrice of'
  = TWOIPriceFixed !(of' GYRational)
  | -- | Price given as a delta to oracle price.
    TWOIPriceDynamic
      { twoioPriceDelta :: !(of' PriceDelta),
        twoioOracleKey :: !GYPaymentVerificationKey
      }

deriving stock instance (Show (of' GYRational), Show (of' PriceDelta)) => Show (TWOIPrice of')

deriving stock instance (Eq (of' GYRational), Eq (of' PriceDelta)) => Eq (TWOIPrice of')

deriving stock instance
  (Generic (of' GYRational), Generic (of' PriceDelta))
  => Generic (TWOIPrice of')

deriving anyclass instance
  (Generic (of' GYRational), Generic (of' PriceDelta), Swagger.ToSchema (of' GYRational), Swagger.ToSchema (of' PriceDelta))
  => Swagger.ToSchema (TWOIPrice of')

deriving anyclass instance
  (Aeson.ToJSON (of' GYRational), Aeson.ToJSON (of' PriceDelta), Generic (of' GYRational), Generic (of' PriceDelta))
  => Aeson.ToJSON (TWOIPrice of')

--------------------------------------------------------------------------------
-- Oracle helpers
--------------------------------------------------------------------------------

data OracleRedeemerPayload = OracleRedeemerPayload
  { orpPrice :: !GYRational,
    orpBaseAsset :: !GYAssetClass,
    orpQuoteAsset :: !GYAssetClass,
    orpTimestampMs :: !Integer,
    orpSignature :: !SignatureOffchain
  }
  deriving stock (Eq, Show)

data PricingModel m = PricingModel
  { pmStraight :: Natural -> m Natural,
    pmReverse :: Natural -> m Natural,
    pmRedeemerPayload :: Maybe OracleRedeemerPayload
  }

oraclePayloadFromCertificate :: OracleCertificate -> OracleRedeemerPayload
oraclePayloadFromCertificate OracleCertificate {ocPrice = Price price, ocBaseAsset, ocQuoteAsset, ocTimestamp, ocSignature} =
  OracleRedeemerPayload
    { orpPrice = rationalFromGHC price,
      orpBaseAsset = ocBaseAsset,
      orpQuoteAsset = ocQuoteAsset,
      orpTimestampMs = floor (utcTimeToPOSIXSeconds ocTimestamp * 1000),
      orpSignature = ocSignature
    }

oracleRedeemerData :: Map GYTokenName OracleRedeemerPayload -> PlutusTx.Data
oracleRedeemerData mp =
  let entries = fmap encodeEntry (Map.toList mp)
   in PlutusTx.Constr 0 [PlutusTx.List entries]
 where
  encodeEntry :: (GYTokenName, OracleRedeemerPayload) -> PlutusTx.Data
  encodeEntry (tn, payload) =
    PlutusTx.List
      [ PlutusTx.toData (tokenNameToPlutus tn),
        oracleFillDetailsData payload
      ]

  oracleFillDetailsData :: OracleRedeemerPayload -> PlutusTx.Data
  oracleFillDetailsData OracleRedeemerPayload {..} =
    let
      priceTimestamp =
        PlutusTx.Constr
          0
          [ assetClassData orpBaseAsset,
            assetClassData orpQuoteAsset,
            rationalData orpPrice,
            PlutusTx.I orpTimestampMs
          ]
      signatureData = PlutusTx.toData (Builtins.toBuiltin (getSignatureOffchain orpSignature))
     in
      PlutusTx.Constr 0 [signatureData, priceTimestamp]

  rationalData :: GYRational -> PlutusTx.Data
  rationalData r =
    let
      rat = rationalToGHC r
      num = numerator rat
      den = denominator rat
     in
      PlutusTx.Constr 0 [PlutusTx.I num, PlutusTx.I den]

  assetClassData :: GYAssetClass -> PlutusTx.Data
  assetClassData ac =
    case assetClassToPlutus ac of
      Ledger.AssetClass (Ledger.CurrencySymbol policy, Ledger.TokenName assetName) ->
        PlutusTx.Constr
          0
          [ PlutusTx.B (Builtins.fromBuiltin policy),
            PlutusTx.B (Builtins.fromBuiltin assetName)
          ]

applyPriceDelta :: GYRational -> PriceDelta -> GYRational
applyPriceDelta base PriceDelta {offset, spread} =
  rationalFromGHC $ (rationalToGHC base * (1 + rationalToGHC spread)) + rationalToGHC offset

ceilingPriceProduct :: Text -> GYRational -> Natural -> Either Text Natural
ceilingPriceProduct context price amt =
  let result = ceiling (rationalToGHC price * toRational amt)
   in if result < 0
        then Left $ context <> ": negative payment derived"
        else Right (fromInteger result)

--------------------------------------------------------------------------------
-- Placement specifications (master API)
--------------------------------------------------------------------------------

-- | Price specification for placing orders via the master function.
data TWPriceSpec
  = TWPriceFixed
      { -- | price for A->B
        twpsStraight :: !GYRational,
        -- | optional price for B->A (two-way only). If 'Nothing', uses reciprocal of 'twpsStraight'.
        twpsReverse :: !(Maybe GYRational)
      }
  | TWPriceRelative
      { twpsOracleVKey :: !GYPaymentVerificationKey,
        -- | delta for straight A->B
        twpsStraightDelta :: !PriceDelta,
        -- | optional delta for reverse B->A; if 'Nothing', uses straight delta.
        twpsReverseDelta :: !(Maybe PriceDelta)
      }
  deriving stock (Eq, Generic, Show)

-- | Direction/deposit specification (one-way vs true two-way).
data TWDirectionSpec
  = TWOneWay
      { -- | amount and offered asset (A)
        twdOffer :: !(Natural, GYAssetClass),
        -- | asked asset (B)
        twdAsk :: !GYAssetClass,
        -- | additional maker fee in offered tokens (A). Use 0 for default.
        twdAddOfferedFee :: !Natural
      }
  | TWTwoway
      { twdOfferA :: !(Natural, GYAssetClass),
        twdOfferB :: !(Natural, GYAssetClass)
      }
  deriving stock (Eq, Generic, Show)

-- | Complete placement spec for a single TWO order.
data TWPlaceSpec = TWPlaceSpec
  { twpsOwner :: !GYAddress,
    twpsDirection :: !TWDirectionSpec,
    twpsPriceSpec :: !TWPriceSpec,
    twpsStart :: !(Maybe GYTime),
    twpsEnd :: !(Maybe GYTime),
    -- | additional lovelace to deposit (maker flat top-up)
    twpsAddLov :: !Natural,
    twpsStakeCred :: !(Maybe GYStakeCredential)
  }
  deriving stock (Eq, Generic, Show)

data Offer price = Offer
  { amount :: !Natural,
    price :: !price
  }

deriving stock instance Show p => Show (Offer p)

deriving stock instance Eq p => Eq (Offer p)

deriving stock instance Generic p => Generic (Offer p)

deriving anyclass instance
  (Generic p, Swagger.ToSchema p)
  => Swagger.ToSchema (Offer p)

deriving anyclass instance (Aeson.ToJSON p, Generic p) => Aeson.ToJSON (Offer p)

-- Note that if you would not like order to be closed when completely filled,
-- set `reverse` to some non-`None` value such that it is detrimental to fill
-- it in reverse direction.
data TwoWays offer = TwoWays
  { straight :: !(AssetDetails offer),
    reverse :: !(AssetDetails (Maybe offer))
  }

deriving stock instance Show o => Show (TwoWays o)

deriving stock instance Eq o => Eq (TwoWays o)

deriving stock instance Generic o => Generic (TwoWays o)

deriving anyclass instance (Generic o, Swagger.ToSchema o) => Swagger.ToSchema (TwoWays o)

deriving anyclass instance (Aeson.ToJSON o, Generic o) => Aeson.ToJSON (TwoWays o)

-- FIXME: Check if it's fine that resulting price due to offset & spread can be negative.

{- | Price delta to be applied to oracle price input to get for final price.
Actual price is `o * (1 + spread) + offset` where `o` is the oracle price.
-}
data PriceDelta = PriceDelta
  { offset :: !GYRational,
    spread :: !GYRational
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

-- | Details of an asset involved in an order.
data AssetDetails offer = AssetDetails
  { -- | Asset class.
    asset :: !GYAssetClass,
    -- | Available offer for this asset.
    offer :: !offer
  }

deriving stock instance Show p => Show (AssetDetails p)

deriving stock instance Eq p => Eq (AssetDetails p)

deriving stock instance Generic p => Generic (AssetDetails p)

deriving anyclass instance
  (Generic p, Swagger.ToSchema p)
  => Swagger.ToSchema (AssetDetails p)

deriving anyclass instance (Aeson.ToJSON p, Generic p) => Aeson.ToJSON (AssetDetails p)

type TwoWayOrderDatum = BPgeniusyield_dex_v2_types_order_OrderDatum

twoWayOrderAddr :: forall m. GYTxQueryMonad m => GYAssetClass -> m GYAddress
twoWayOrderAddr reNft = do
  -- gycs <- ask -- ???: should add Aiken scripts to Ctx as well?
  scriptAddress @m @PlutusV3 $ twoWayOrderSpendValidator reNft

twoWayOrderNftPolicyId :: GYAssetClass -> GYMintingPolicyId
twoWayOrderNftPolicyId reNft = mintingPolicyId $ twoWayOrderMintValidator reNft

twoWayOrders
  :: forall m
   . GYApiQueryMonad m
  => TWORef
  -> m (Map GYTxOutRef TwoWayOrderInfo)
twoWayOrders twor = do
  addr <- twoWayOrderAddr twor.tworRefNft
  payCred <-
    maybe (throwAppError $ someBackendError "two-way order script address missing payment credential") pure $
      addressToPaymentCredential addr
  utxosWithDatums <- utxosAtPaymentCredentialWithDatums payCred Nothing
  twoWayOrderNftPolicyId twor.tworRefNft & mkTWOrderInfo & iwither $
    utxosDatumsPureWithOriginalDatum utxosWithDatums
 where
  mkTWOrderInfo
    :: GYMintingPolicyId
    -> GYTxOutRef
    -> (GYAddress, GYValue, TwoWayOrderDatum, GYDatum)
    -> m (Maybe TwoWayOrderInfo)
  mkTWOrderInfo policyId orderRef tuple =
    pure empty & const & catchError do
      Just <$> makeTwoWayOrderInfo policyId orderRef tuple

twoWayOrdersWithTransformerPredicate
  :: GYApiQueryMonad m
  => TWORef
  -> (TwoWayOrderInfo -> Maybe b)
  -> m [b]
twoWayOrdersWithTransformerPredicate twors tp = do
  ois <- Map.elems <$> twoWayOrders twors
  pure $ mapMaybe tp ois

makeTwoWayOrderInfo
  :: forall m
   . GYApiQueryMonad m
  => GYMintingPolicyId
  -> GYTxOutRef
  -> (GYAddress, GYValue, TwoWayOrderDatum, GYDatum)
  -> m TwoWayOrderInfo
makeTwoWayOrderInfo policyId orderRef (utxoAddr, v, twoDatum, origDatum) = do
  let BPgeniusyield_dex_v2_types_order_OrderDatum0OrderDatum
        (credentialsFromMultisig -> ownerCredentials)
        ( \case
            BPcardano_address_Address0Address
              (credentialFromBPPayment -> credential)
              ( \case
                  BPOption_StakeCredential0Some
                    ( \case
                        BPStakeCredential0Inline
                          (credentialFromCardanoAddress -> credential') ->
                            StakingHash credential'
                        BPStakeCredential1Pointer x y z -> StakingPtr x y z ->
                        stakingCredential
                      ) -> Just stakingCredential
                  BPOption_StakeCredential1None -> Nothing ->
                  staking
                ) ->
                Address credential staking ->
            paymentAddr
          )
        (tokenName . fromBuiltin -> nft)
        (assetToPlutus -> strAsset)
        (assetToPlutus -> revAsset)
        ( \case
            BPgeniusyield_dex_v2_types_order_Price0Fixed
              (rationalToPlutus' -> stright)
              ( \case
                  BPOption_geniusyield_dex_v2_types_rational_Rational1None -> Nothing
                  BPOption_geniusyield_dex_v2_types_rational_Rational0Some
                    (rationalToPlutus' -> o) -> Just o ->
                  revers
                ) -> Left (stright, revers)
            BPgeniusyield_dex_v2_types_order_Price1DeltaOracle
              (fromBuiltin -> verifKey)
              (deltaToPlutus -> stright)
              ( \case
                  BPOption_geniusyield_dex_v2_types_order_PriceDelta1None -> Nothing
                  BPOption_geniusyield_dex_v2_types_order_PriceDelta0Some
                    (deltaToPlutus -> priceDelta) -> Just priceDelta ->
                  revers
                ) -> Right (verifKey, stright, revers) ->
            price
          )
        (posixTimeFromTimestamp -> validityStart)
        (posixTimeFromTimestamp -> validityEnd)
        takerLovelaceFlatFee
        (rationalToPlutus' -> takerFeeRatio)
        (rationalToPlutus' -> makerFeeRatio)
        (fromInteger -> freshnessSeconds) =
          twoDatum
  twoiOwnerAddr <- addressFromPlutus' paymentAddr
  twoiOwnerCredentials <- for ownerCredentials \case
    PubKeyCredential pkh' ->
      GYCredentialByKey . fromPubKeyHash
        <$> pubKeyHashFromPlutus' pkh'
    ScriptCredential sh' ->
      validatorHashFromPlutus sh' & do
        pure . GYCredentialByScript & either do
          throwError . GYConversionException . GYLedgerToCardanoError
  twoiNFT <- tokenNameFromPlutus' nft
  twoiOffer <- case price of
    Left (s, r) -> do
      sa <-
        assetClassFromPlutus (fst strAsset) & do
          pure . AssetDetails & either do
            throwError . GYConversionException . GYInvalidPlutusAsset
      ra <-
        assetClassFromPlutus (fst revAsset) & do
          pure . AssetDetails & either do
            throwError . GYConversionException . GYInvalidPlutusAsset
      (sa . Offer (snd strAsset) -> strDetails) <- rationalFromPlutus' s
      (ra . fmap (Offer (snd revAsset)) -> revDetails) <- rationalFromPlutus' & for r
      pure . TWOIPriceFixed . TWOrder $ TwoWays strDetails revDetails
    Right (k, s, r) -> do
      key <-
        signingKeyFromRawBytes k & do
          pure . paymentVerificationKey & maybe do
            throwError . GYConversionException . GYLedgerToCardanoError $
              DeserialiseRawBytesError "Malformed VerificationKey"
      sa <-
        assetClassFromPlutus (fst strAsset) & do
          pure . AssetDetails & either do
            throwError . GYConversionException . GYInvalidPlutusAsset
      ra <-
        assetClassFromPlutus (fst revAsset) & do
          pure . AssetDetails & either do
            throwError . GYConversionException . GYInvalidPlutusAsset
      (sa . Offer (snd strAsset) -> strDetails) <- do
        offset <- rationalFromPlutus' $ fst <$> s
        spread <- rationalFromPlutus' $ snd <$> s
        pure PriceDelta {..}
      (ra . fmap (Offer (snd revAsset)) -> revDetails) <- for r \r' -> do
        offset <- rationalFromPlutus' $ fst <$> r'
        spread <- rationalFromPlutus' $ snd <$> r'
        pure PriceDelta {..}
      pure $
        TWOIPriceDynamic
          { twoioPriceDelta = TWOrder (TwoWays strDetails revDetails),
            twoioOracleKey = key
          }
  twoiTakerFeeRatio <- rationalFromPlutus' takerFeeRatio
  twoiMakerFeeRatio <- rationalFromPlutus' makerFeeRatio

  let
    twoiRef = orderRef
    -- \*** --
    twoiStart = timeFromPlutus <$> validityStart
    twoiEnd = timeFromPlutus <$> validityEnd
    -- fees --
    twoiTakerLovelaceFlatFee = fromInteger takerLovelaceFlatFee
    twoiOracleFreshnessSeconds = fromInteger freshnessSeconds
    -- \*** --
    twoiUTxOValue = v
    twoiUTxOAddr = utxoAddr
    twoiNFTCS = policyId
    twoiRawDatum = origDatum
  pure TwoWayOrderInfo {..}
 where
  deltaToPlutus
    :: BPgeniusyield_dex_v2_types_order_PriceDelta
    -> Maybe (Tx.Rational, Tx.Rational)
  deltaToPlutus = \case
    BPgeniusyield_dex_v2_types_order_PriceDelta0PriceDelta
      (rationalToPlutus' -> offset)
      (rationalToPlutus' -> spread) -> do
        o <- offset
        s <- spread
        pure (o, s)
  rationalFromPlutus' :: Maybe Tx.Rational -> m GYRational
  rationalFromPlutus' =
    pure . rationalFromPlutus & maybe do
      throwError . GYConversionException . GYLedgerToCardanoError $
        DeserialiseRawBytesError "Malformed Rational"
  rationalToPlutus' :: BPgeniusyield_dex_v2_types_rational_Rational -> Maybe Tx.Rational
  rationalToPlutus' = \case
    BPgeniusyield_dex_v2_types_rational_Rational0Rational n d -> Tx.ratio n d
  posixTimeFromTimestamp :: BPOption_Int -> Maybe POSIXTime
  posixTimeFromTimestamp = \case
    BPOption_Int1None -> Nothing
    BPOption_Int0Some time -> Just $ POSIXTime time
  credentialFromBPPayment :: BPPaymentCredential -> Credential
  credentialFromBPPayment = \case
    BPPaymentCredential0VerificationKey vkh ->
      PubKeyCredential $ PubKeyHash vkh
    BPPaymentCredential1Script sh ->
      ScriptCredential $ ScriptHash sh
  credentialFromCardanoAddress :: BPcardano_address_Credential -> Credential
  credentialFromCardanoAddress = \case
    BPcardano_address_Credential0VerificationKey vkh ->
      PubKeyCredential $ PubKeyHash vkh
    BPcardano_address_Credential1Script sh ->
      ScriptCredential $ ScriptHash sh
  credentialsFromMultisig
    :: BPgeniusyield_dex_v2_types_multisig_MultisigScript -> [Credential]
  credentialsFromMultisig = \case
    BPgeniusyield_dex_v2_types_multisig_MultisigScript0MultisigScript
      verificationKeyHashes
      scriptHashes ->
        do PubKeyCredential . PubKeyHash <$> verificationKeyHashes
          <> do ScriptCredential . ScriptHash <$> scriptHashes
  assetToPlutus
    :: BPgeniusyield_dex_v2_types_order_AssetDetails -> (AssetClass, Natural)
  assetToPlutus = \case
    BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails
      ( \case
          BPgeniusyield_dex_v2_types_assets_AssetClass0AssetClass
            policy
            assetName ->
              AssetClass (Ledger.CurrencySymbol policy, Ledger.TokenName assetName) ->
          asset
        )
      (fromInteger -> amount) -> (asset, amount)

-- spend tx hash into 'twoWayOrderMintValidator'

fillTwoWayOrders
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> NonEmpty TWFillSpec
  -> RefTWOCD
  -> m (GYTxSkeleton PlutusV3)
fillTwoWayOrders twors specs (RefTWOCD (cfgRef :!: twocd)) = do
  nid <- networkId
  let
    fillStakeCredential = GYStakeCredentialByScript twocd.twocdFillHash
    fillStakeAddress = stakeAddressFromCredential nid fillStakeCredential
    mintScript = mintingPolicyToScript $ twoWayOrderMintValidator twors.tworRefNft

  finalStates <- foldM (processFillSpec twors twocd) Map.empty (NE.toList specs)

  (combinedSkeleton, takerFeeAcc, makerFeeAcc, maxFlat, anyConsumed, oracleMap, mDeadlineSlot) <-
    foldM
      (accumulateFillState twocd)
      (mempty, mempty, mempty, 0, False, Map.empty, Nothing)
      (Map.elems finalStates)

  gyLogDebug'
    "TWO.fill.feeTotals"
    ( "taker="
        ++ show takerFeeAcc
        ++ ", maker="
        ++ show makerFeeAcc
    )

  let
    takerPositive = positiveOnly takerFeeAcc
    feeWithoutAda = takerPositive <> makerFeeAcc
    requiresTakerFlat = (feeWithoutAda /= mempty || anyConsumed) && maxFlat > 0
    feeValue =
      feeWithoutAda
        <> if requiresTakerFlat then valueSingleton GYLovelace maxFlat else mempty

  gyLogDebug'
    "TWO.fill.feeSummary"
    ( "takerPositive="
        ++ show takerPositive
        ++ ", makerAcc="
        ++ show makerFeeAcc
        ++ ", feeValue="
        ++ show feeValue
    )

  let
    feeOutput
      | feeValue == mempty = mempty
      | otherwise =
          mustHaveOutput
            GYTxOut
              { gyTxOutAddress = twocd.twocdFeeAddr,
                gyTxOutValue = feeValue,
                gyTxOutDatum = Just (datumFromPlutusData $ scriptPlutusHash mintScript, GYTxOutUseInlineDatum),
                gyTxOutRefS = Nothing
              }

    fillStakeRedeemer =
      redeemerFromPlutus' $
        PlutusTx.dataToBuiltinData (oracleRedeemerData oracleMap)
    fillWithdrawal =
      mustHaveWithdrawal
        GYTxWdrl
          { gyTxWdrlStakeAddress = fillStakeAddress,
            gyTxWdrlAmount = 0,
            gyTxWdrlWitness =
              GYTxWdrlWitnessScript
                (GYStakeValReference twors.tworFillRef $ twoWayOrderFillValidator twors.tworRefNft)
                fillStakeRedeemer
          }

  gyLogDebug' "TWO.fill.feeOutputValue" (show feeValue)

  gyLogDebug' "TWO.fill.oracleMap" (show oracleMap)
  gyLogDebug'
    "TWO.fill.oracleRedeemerData"
    (show $ oracleMap & oracleRedeemerData)

  let
    baseSkeleton =
      fillWithdrawal
        <> combinedSkeleton
        <> feeOutput
        <> mustHaveRefInput cfgRef
        <> mustHaveTxMetadata stampFilled
    deadlineConstraint = maybe mempty isInvalidAfter mDeadlineSlot
  pure $ baseSkeleton <> deadlineConstraint

fillTwoWayAndLegacyPartialOrders
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> [TWFillSpec]
  -> RefTWOCD
  -> PORefs
  -> [(GYTxOutRef, Natural)]
  -> Maybe RefPocds
  -> m (GYTxSkeleton PlutusV3)
fillTwoWayAndLegacyPartialOrders twor specs refTwo pors partials mRefPocd = do
  twoWaySkeleton <-
    case NE.nonEmpty specs of
      Just neSpecs -> fillTwoWayOrders twor neSpecs refTwo
      Nothing -> pure mempty
  partialSkeletonV2 <-
    case partials of
      [] -> pure mempty
      _ -> fillMultiplePartialOrders pors partials mRefPocd
  let partialSkeletonV3 = upgradeTxSkeleton partialSkeletonV2 :: GYTxSkeleton PlutusV3
  pure $ twoWaySkeleton <> partialSkeletonV3

type FillAccumulator =
  ( GYTxSkeleton PlutusV3,
    GYValue, -- taker fee accumulator (can include negatives)
    GYValue, -- maker fee accumulator
    Integer,
    Bool,
    Map.Map GYTokenName OracleRedeemerPayload,
    Maybe GYSlot
  )

data FillState = FillState
  { fsInfo :: TwoWayOrderInfo,
    fsOrderDatum :: BPgeniusyield_dex_v2_types_order_OrderDatum,
    fsStraightAsset :: GYAssetClass,
    fsReverseAsset :: GYAssetClass,
    fsInitialStraight :: Integer,
    fsInitialReverse :: Integer,
    fsCurrentStraight :: Integer,
    fsCurrentReverse :: Integer,
    fsSkeleton :: GYTxSkeleton PlutusV3,
    fsFlatFee :: Integer,
    fsConsumed :: Bool,
    fsOraclePayload :: Maybe OracleRedeemerPayload,
    fsDeadline :: Maybe GYSlot,
    fsTakerFeeValue :: GYValue,
    fsMakerFeeValue :: GYValue
  }

data OrderAssets = OrderAssets
  { oaStraightAsset :: !GYAssetClass,
    oaStraightAmount :: !Integer,
    oaReverseAsset :: !GYAssetClass,
    oaReverseAmount :: !Integer
  }
  deriving (Eq, Show)

extractOrderAssets :: TwoWayOrderInfo -> OrderAssets
extractOrderAssets TwoWayOrderInfo {twoiOffer} =
  case twoiOffer of
    TWOIPriceFixed (TWOrder twoWays) ->
      orderAssetsFromTwoWays twoWays
    TWOIPriceDynamic {twoioPriceDelta = TWOrder twoWays} ->
      orderAssetsFromTwoWays twoWays
 where
  orderAssetsFromTwoWays
    TwoWays
      { straight = AssetDetails {asset = straightAsset, offer = straightOffer},
        reverse = AssetDetails {asset = reverseAsset, offer = reverseOffer}
      } =
      OrderAssets
        { oaStraightAsset = straightAsset,
          oaStraightAmount = toInteger straightOffer.amount,
          oaReverseAsset = reverseAsset,
          oaReverseAmount = maybe 0 (\offer -> toInteger offer.amount) reverseOffer
        }

data PriceVal = FixedVal GYRational | DynamicVal PriceDelta deriving (Eq, Show)

data OrderPrices = OrderPrices {opStraightPrice :: PriceVal, opReversePrice :: Maybe PriceVal} deriving (Eq, Show)

extractOrderPrices :: TwoWayOrderInfo -> OrderPrices
extractOrderPrices TwoWayOrderInfo {twoiOffer} =
  case twoiOffer of
    TWOIPriceFixed (TWOrder twoWays) ->
      case twoWays of
        TwoWays {straight = AssetDetails _ straightOffer, reverse = AssetDetails _ mReverseOffer} ->
          OrderPrices
            { opStraightPrice = FixedVal (straightOffer.price),
              opReversePrice = if isJust mReverseOffer then Just (FixedVal (fromJust mReverseOffer).price) else Nothing
            }
    TWOIPriceDynamic {twoioPriceDelta = TWOrder twoWays} ->
      case twoWays of
        TwoWays {straight = AssetDetails _ straightOffer, reverse = AssetDetails _ mReverseOffer} ->
          OrderPrices
            { opStraightPrice = DynamicVal (straightOffer.price),
              opReversePrice = if isJust mReverseOffer then Just (DynamicVal (fromJust mReverseOffer).price) else Nothing
            }

data PricingContext m = PricingContext
  { pcStraightAsset :: GYAssetClass,
    pcReverseAsset :: GYAssetClass,
    pcPricingModel :: PricingModel m,
    pcOraclePayload :: Maybe OracleRedeemerPayload,
    pcDeadline :: Maybe GYSlot
  }

processFillSpec
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> TwoWayOrderConfigDatumF GYAddress
  -> Map.Map GYTxOutRef FillState
  -> TWFillSpec
  -> m (Map.Map GYTxOutRef FillState)
processFillSpec twors twocd states spec@TWFillSpec {..} = do
  gyLogDebug'
    "TWO.fill.fragment"
    ( "direction="
        ++ show twfsDirection
        ++ ", ref="
        ++ show twfsOrderRef
        ++ ", amount="
        ++ show twfsAmount
    )
  when (twfsAmount == 0) $ throwAppError $ someBackendError "fill amount must be positive"
  case Map.lookup twfsOrderRef states of
    Just st -> do
      gyLogDebug' "TWO.fill.offer" (show st.fsInfo.twoiOffer)
      pricingCtx <- resolvePricingContext st.fsInfo spec
      st' <- applyFillState twocd st spec pricingCtx
      pure $ Map.insert twfsOrderRef st' states
    Nothing -> do
      twoi <- getTwoWayOrderInfo twors twfsOrderRef
      gyLogDebug' "TWO.fill.offer" (show twoi.twoiOffer)
      orderDatum <- decodeOrderDatum twoi
      pricingCtx <- resolvePricingContext twoi spec
      let
        orderAssets = extractOrderAssets twoi
        initialStraight = selectInitialAmount twoi orderAssets pricingCtx.pcStraightAsset
        initialReverse = selectInitialAmount twoi orderAssets pricingCtx.pcReverseAsset
        baseSkeleton =
          mustHaveInput
            (twoWayOrderInfoToIn twors twoi BPgeniusyield_dex_v2_types_order_OrderRedeemer1FillOrder)
        baseState =
          FillState
            { fsInfo = twoi,
              fsOrderDatum = orderDatum,
              fsStraightAsset = pricingCtx.pcStraightAsset,
              fsReverseAsset = pricingCtx.pcReverseAsset,
              fsInitialStraight = initialStraight,
              fsInitialReverse = initialReverse,
              fsCurrentStraight = initialStraight,
              fsCurrentReverse = initialReverse,
              fsSkeleton = baseSkeleton,
              fsFlatFee = 0,
              fsConsumed = False,
              fsOraclePayload = Nothing,
              fsDeadline = Nothing,
              fsTakerFeeValue = mempty,
              fsMakerFeeValue = mempty
            }
      st' <- applyFillState twocd baseState spec pricingCtx
      pure $ Map.insert twfsOrderRef st' states
     where
      selectInitialAmount
        twoi'
        OrderAssets
          { oaStraightAsset = straightAsset,
            oaStraightAmount = straightAmount,
            oaReverseAsset = reverseAsset,
            oaReverseAmount = reverseAmount
          }
        targetAsset
          | targetAsset == straightAsset = straightAmount
          | targetAsset == reverseAsset = reverseAmount
          | otherwise = amountOf twoi'.twoiUTxOValue targetAsset

accumulateFillState
  :: (GYApiMonad m, HasCallStack)
  => TwoWayOrderConfigDatumF GYAddress
  -> FillAccumulator
  -> FillState
  -> m FillAccumulator
accumulateFillState twocd (skAcc, takerAcc, makerAcc, flatAcc, consumedAcc, oracleAcc, deadlineAcc) st = do
  continuingOut <-
    buildContinuingOutput
      st.fsOrderDatum
      st.fsInfo.twoiUTxOAddr
      st.fsStraightAsset
      st.fsReverseAsset
      st.fsInitialStraight
      st.fsInitialReverse
      st.fsCurrentStraight
      st.fsCurrentReverse
      st.fsInfo
      twocd
  oracleAcc' <- mergeOracleMap oracleAcc st.fsInfo.twoiNFT st.fsOraclePayload
  let
    (orderTakerFee, orderMakerFee) = calculateOrderFees twocd st
    skAcc' = skAcc <> st.fsSkeleton <> mustHaveOutput continuingOut
    takerAcc' = takerAcc <> orderTakerFee
    makerAcc' = makerAcc <> orderMakerFee
    flatAcc' = max flatAcc st.fsFlatFee
    consumedAcc' = consumedAcc || st.fsConsumed
    deadlineAcc' = mergeDeadlines deadlineAcc st.fsDeadline
  pure (skAcc', takerAcc', makerAcc', flatAcc', consumedAcc', oracleAcc', deadlineAcc')

resolvePricingContext
  :: (GYApiMonad m, HasCallStack)
  => TwoWayOrderInfo
  -> TWFillSpec
  -> m (PricingContext m)
resolvePricingContext twoi TWFillSpec {..} = do
  case twoi.twoiOffer of
    TWOIPriceFixed (TWOrder twoWays) -> do
      let
        straightDetails = twoWays.straight
        reverseDetails = twoWays.reverse
        straightAsset = straightDetails.asset
        reverseAsset = reverseDetails.asset
        straightOffer = straightDetails.offer
        maybeReverseOffer = reverseDetails.offer
        straightCalc amt = pure $ orderPrice straightOffer.price amt
        reverseCalc amt = case maybeReverseOffer of
          Nothing -> throwAppError $ someBackendError "order does not support reverse fills"
          Just offer -> pure $ orderPrice offer.price amt
      pure
        PricingContext
          { pcStraightAsset = straightAsset,
            pcReverseAsset = reverseAsset,
            pcPricingModel =
              PricingModel
                { pmStraight = straightCalc,
                  pmReverse = reverseCalc,
                  pmRedeemerPayload = Nothing
                },
            pcOraclePayload = Nothing,
            pcDeadline = Nothing
          }
    TWOIPriceDynamic {twoioPriceDelta = TWOrder deltaWays} -> do
      cert <-
        maybe
          (throwAppError $ someBackendError "oracle certificate required for dynamic pricing fill")
          pure
          twfsOracleCertificate
      let
        payload = oraclePayloadFromCertificate cert
        rawPrice = payload.orpPrice
        straightDetails = deltaWays.straight
        reverseDetails = deltaWays.reverse
        straightAsset = straightDetails.asset
        reverseAsset = reverseDetails.asset
        straightDelta = straightDetails.offer.price
        maybeReverseDelta = fmap (.price) reverseDetails.offer
        straightPairOk =
          payload.orpBaseAsset == straightAsset
            && payload.orpQuoteAsset == reverseAsset
        reversePairOk =
          payload.orpBaseAsset == reverseAsset
            && payload.orpQuoteAsset == straightAsset
      gyLogDebug' "TWO.fill.oracle.baseAsset" ("base=" ++ show payload.orpBaseAsset)
      gyLogDebug' "TWO.fill.oracle.quoteAsset" ("quote=" ++ show payload.orpQuoteAsset)
      gyLogDebug' "TWO.fill.oracle.rawPrice" ("rawPrice=" ++ show rawPrice)
      deadlineSlot <-
        let
          freshnessMs = toInteger twoi.twoiOracleFreshnessSeconds * 1000
          expiryMs = payload.orpTimestampMs + freshnessMs
          expiryPosix = fromRational (expiryMs % 1000)
         in
          enclosingSlotFromTime' (timeFromPOSIX expiryPosix)
      case twfsDirection of
        FillDirectionStraight -> do
          unless straightPairOk $
            throwAppError $
              someBackendError "oracle certificate asset pair mismatch for straight fill"
          let
            straightPrice = applyPriceDelta rawPrice straightDelta
            straightCalc amt =
              either (throwAppError . someBackendError) pure $
                ceilingPriceProduct "straight payment" straightPrice amt
            reverseCalc _ =
              throwAppError $
                someBackendError "reverse pricing requested without matching oracle certificate"
          gyLogDebug' "TWO.fill.oracle.basePrice" ("basePrice=" ++ show straightPrice)
          pure
            PricingContext
              { pcStraightAsset = straightAsset,
                pcReverseAsset = reverseAsset,
                pcPricingModel =
                  PricingModel
                    { pmStraight = straightCalc,
                      pmReverse = reverseCalc,
                      pmRedeemerPayload = Just payload
                    },
                pcOraclePayload = Just payload,
                pcDeadline = Just deadlineSlot
              }
        FillDirectionReverse -> do
          unless reversePairOk $
            throwAppError $
              someBackendError "oracle certificate asset pair mismatch for reverse fill"
          reverseDelta <-
            maybe
              (throwAppError $ someBackendError "order does not support reverse fills under dynamic pricing")
              pure
              maybeReverseDelta
          let
            reversePrice = applyPriceDelta rawPrice reverseDelta
            reverseCalc amt =
              either (throwAppError . someBackendError) pure $
                ceilingPriceProduct "reverse payment" reversePrice amt
            straightCalc _ =
              throwAppError $
                someBackendError "straight pricing requested without matching oracle certificate"
          gyLogDebug' "TWO.fill.oracle.basePrice" ("basePrice=" ++ show reversePrice)
          pure
            PricingContext
              { pcStraightAsset = straightAsset,
                pcReverseAsset = reverseAsset,
                pcPricingModel =
                  PricingModel
                    { pmStraight = straightCalc,
                      pmReverse = reverseCalc,
                      pmRedeemerPayload = Just payload
                    },
                pcOraclePayload = Just payload,
                pcDeadline = Just deadlineSlot
              }

orderPrice :: GYRational -> Natural -> Natural
orderPrice price amt = ceiling $ rationalToGHC price * toRational amt

makerFeeFromNet :: Rational -> Integer -> Integer
makerFeeFromNet ratio net =
  floor (fromIntegral net * ratio)

applyFillState
  :: (GYApiMonad m, HasCallStack)
  => TwoWayOrderConfigDatumF GYAddress
  -> FillState
  -> TWFillSpec
  -> PricingContext m
  -> m FillState
applyFillState twocd st TWFillSpec {..} PricingContext {..} = do
  when (pcStraightAsset /= st.fsStraightAsset || pcReverseAsset /= st.fsReverseAsset) $
    throwAppError $
      someBackendError "inconsistent asset layout across grouped fills"
  let
    PricingModel {..} = pcPricingModel
    currentStraight = st.fsCurrentStraight
    currentReverse = st.fsCurrentReverse
    offerNet = toInteger twfsAmount
    takerFlatFee = toInteger st.fsInfo.twoiTakerLovelaceFlatFee
    mkValue asset amt =
      if amt == 0 then mempty else valueSingleton asset amt
  gyLogDebug'
    "TWO.fill.orderState"
    ( "direction="
        ++ show twfsDirection
        ++ ", currentStraight="
        ++ show currentStraight
        ++ ", currentReverse="
        ++ show currentReverse
        ++ ", offerNet="
        ++ show offerNet
    )
  case twfsDirection of
    FillDirectionStraight -> do
      when (currentStraight < 0) $ throwAppError $ someBackendError "negative straight liquidity"
      let
        makerRatio = rationalToGHC (twocd.twocdMakerFeeRatio)
        takerRatio = rationalToGHC (twocd.twocdTakerFeeRatio)
        makerFeeOffer = makerFeeFromNet makerRatio offerNet
        offerGross = offerNet + makerFeeOffer
      when (offerGross > currentStraight) $ throwAppError $ someBackendError "fill amount exceeds available straight liquidity"
      paymentProvidedNat <- pmStraight twfsAmount
      let
        paymentProvided = toInteger paymentProvidedNat
        newStraight = currentStraight - offerGross
        newReverse = currentReverse + paymentProvided
      when (newStraight < 0) $ throwAppError $ someBackendError "insufficient straight liquidity"
      let
        takerFeePayment = floor (toRational paymentProvided * takerRatio) :: Integer
        takerTokenAmount = offerNet
        takerFeeOffer = floor (toRational offerNet * takerRatio) :: Integer
      when (takerTokenAmount < 0) $ throwAppError $ someBackendError "maker fee exceeds amount taken"
      let
        takerValue = mkValue pcStraightAsset takerTokenAmount
        takerSkeleton =
          if takerValue == mempty
            then mempty
            else
              mustHaveOutput
                GYTxOut
                  { gyTxOutAddress = twfsRecipient,
                    gyTxOutValue = takerValue,
                    gyTxOutDatum = Nothing,
                    gyTxOutRefS = Nothing
                  }
        consumedAny = offerGross > 0 || paymentProvided > 0
        takerContribution =
          mkValue pcReverseAsset takerFeePayment
            <> mkValue pcStraightAsset (negate takerFeeOffer)
        makerContribution = mkValue pcStraightAsset makerFeeOffer
      gyLogDebug'
        "TWO.fill.straight"
        ( "continuing="
            ++ show
              ( valueSingleton pcStraightAsset newStraight
                  <> valueSingleton pcReverseAsset newReverse
              )
            ++ ", takerFlat="
            ++ show takerFlatFee
            ++ ", makerFeeOffer="
            ++ show makerFeeOffer
            ++ ", takerFeePayment="
            ++ show takerFeePayment
            ++ ", takerFeeOffer="
            ++ show takerFeeOffer
        )
      gyLogDebug'
        "TWO.fill.straight.fees"
        ( "makerContribution="
            ++ show makerContribution
            ++ ", takerContribution="
            ++ show takerContribution
        )
      oraclePayload' <- mergeOraclePayloads st.fsOraclePayload pcOraclePayload
      let deadline' = mergeDeadlines st.fsDeadline pcDeadline
      pure
        st
          { fsCurrentStraight = newStraight,
            fsCurrentReverse = newReverse,
            fsSkeleton = st.fsSkeleton <> takerSkeleton,
            fsFlatFee = max st.fsFlatFee (if consumedAny then takerFlatFee else 0),
            fsConsumed = st.fsConsumed || consumedAny,
            fsOraclePayload = oraclePayload',
            fsDeadline = deadline',
            fsTakerFeeValue = st.fsTakerFeeValue <> takerContribution,
            fsMakerFeeValue = st.fsMakerFeeValue <> makerContribution
          }
    FillDirectionReverse -> do
      when (currentReverse < 0) $ throwAppError $ someBackendError "negative reverse liquidity"
      let
        makerRatio = rationalToGHC (twocd.twocdMakerFeeRatio)
        takerRatio = rationalToGHC (twocd.twocdTakerFeeRatio)
        makerFeeOffer = makerFeeFromNet makerRatio offerNet
        offerGross = offerNet + makerFeeOffer
      when (offerGross > currentReverse) $ throwAppError $ someBackendError "fill amount exceeds available reverse liquidity"
      paymentProvidedNat <- pmReverse twfsAmount
      let
        paymentProvided = toInteger paymentProvidedNat
        newReverse = currentReverse - offerGross
        newStraight = currentStraight + paymentProvided
      when (newReverse < 0) $ throwAppError $ someBackendError "insufficient reverse liquidity"
      let
        takerFeePayment = floor (toRational paymentProvided * takerRatio) :: Integer
        takerTokenAmount = offerNet
        takerFeeOffer = floor (toRational offerNet * takerRatio) :: Integer
      when (takerTokenAmount < 0) $ throwAppError $ someBackendError "maker fee exceeds amount taken"
      let
        takerValue = mkValue pcReverseAsset takerTokenAmount
        takerSkeleton =
          if takerValue == mempty
            then mempty
            else
              mustHaveOutput
                GYTxOut
                  { gyTxOutAddress = twfsRecipient,
                    gyTxOutValue = takerValue,
                    gyTxOutDatum = Nothing,
                    gyTxOutRefS = Nothing
                  }
        consumedAny = offerGross > 0 || paymentProvided > 0
        takerContribution =
          mkValue pcStraightAsset takerFeePayment
            <> mkValue pcReverseAsset (negate takerFeeOffer)
        makerContribution = mkValue pcReverseAsset makerFeeOffer
      gyLogDebug'
        "TWO.fill.reverse"
        ( "continuing="
            ++ show
              ( valueSingleton pcStraightAsset newStraight
                  <> valueSingleton pcReverseAsset newReverse
              )
            ++ ", takerFlat="
            ++ show takerFlatFee
            ++ ", makerFeeOffer="
            ++ show makerFeeOffer
            ++ ", takerFeePayment="
            ++ show takerFeePayment
            ++ ", takerFeeOffer="
            ++ show takerFeeOffer
        )
      gyLogDebug'
        "TWO.fill.reverse.fees"
        ( "makerContribution="
            ++ show makerContribution
            ++ ", takerContribution="
            ++ show takerContribution
        )
      oraclePayload' <- mergeOraclePayloads st.fsOraclePayload pcOraclePayload
      let deadline' = mergeDeadlines st.fsDeadline pcDeadline
      pure
        st
          { fsCurrentStraight = newStraight,
            fsCurrentReverse = newReverse,
            fsSkeleton = st.fsSkeleton <> takerSkeleton,
            fsFlatFee = max st.fsFlatFee (if consumedAny then takerFlatFee else 0),
            fsConsumed = st.fsConsumed || consumedAny,
            fsOraclePayload = oraclePayload',
            fsDeadline = deadline',
            fsTakerFeeValue = st.fsTakerFeeValue <> takerContribution,
            fsMakerFeeValue = st.fsMakerFeeValue <> makerContribution
          }

mergeOraclePayloads
  :: (GYApiMonad m, HasCallStack)
  => Maybe OracleRedeemerPayload
  -> Maybe OracleRedeemerPayload
  -> m (Maybe OracleRedeemerPayload)
mergeOraclePayloads existing incoming =
  case (existing, incoming) of
    (Nothing, payload) -> pure payload
    (payload@(Just _), Nothing) -> pure payload
    (Just a, Just b)
      | a == b -> pure (Just a)
      | otherwise -> pure (Just b)

mergeOracleMap
  :: (GYApiMonad m, HasCallStack)
  => Map.Map GYTokenName OracleRedeemerPayload
  -> GYTokenName
  -> Maybe OracleRedeemerPayload
  -> m (Map.Map GYTokenName OracleRedeemerPayload)
mergeOracleMap oracleAcc _ Nothing = pure oracleAcc
mergeOracleMap oracleAcc tn (Just payload) =
  case Map.lookup tn oracleAcc of
    Nothing -> pure $ Map.insert tn payload oracleAcc
    Just existing
      | existing == payload -> pure oracleAcc
      | otherwise ->
          throwAppError $
            someBackendError $
              "conflicting oracle certificate for order NFT "
                <> Txt.pack (show tn)

mergeDeadlines :: Maybe GYSlot -> Maybe GYSlot -> Maybe GYSlot
mergeDeadlines existing incoming =
  case (existing, incoming) of
    (Nothing, Nothing) -> Nothing
    (Just slot, Nothing) -> Just slot
    (Nothing, Just slot) -> Just slot
    (Just slotA, Just slotB) -> Just (min slotA slotB)

positiveOnly :: GYValue -> GYValue
positiveOnly = valueMake . Map.filter (> 0) . valueToMap

calculateOrderFees
  :: TwoWayOrderConfigDatumF GYAddress
  -> FillState
  -> (GYValue, GYValue)
calculateOrderFees _ FillState {..} = (fsTakerFeeValue, fsMakerFeeValue)

buildContinuingOutput
  :: (GYApiMonad m, HasCallStack)
  => BPgeniusyield_dex_v2_types_order_OrderDatum
  -> GYAddress
  -> GYAssetClass
  -> GYAssetClass
  -> Integer
  -- ^ current straight amount
  -> Integer
  -- ^ current reverse amount
  -> Integer
  -- ^ new straight amount
  -> Integer
  -- ^ new reverse amount
  -> TwoWayOrderInfo
  -> TwoWayOrderConfigDatumF GYAddress
  -> m (GYTxOut PlutusV3)
buildContinuingOutput orderDatum orderAddr straightAsset reverseAsset currentStraight currentReverse newStraight newReverse twoi twocd = do
  continuingDeposit <-
    case resolveContinuingDeposit expectedDeposit currentLovelace of
      Left err -> throwAppError $ someBackendError err
      Right deposit -> pure deposit
  let
    continuingValue =
      currentValue
        <> valueSingleton straightAsset (newStraight - currentStraight)
        <> valueSingleton reverseAsset (newReverse - currentReverse)
        <> valueSingleton GYLovelace (continuingDeposit - currentLovelace)
    continuingOut =
      GYTxOut
        { gyTxOutAddress = orderAddr,
          gyTxOutValue = continuingValue,
          gyTxOutDatum = Just (continuingDatum, GYTxOutUseInlineDatum),
          gyTxOutRefS = Nothing
        }
  gyLogDebug'
    "TWO.fill.continuing"
    ( "deltaStraight="
        ++ show deltaStraight
        ++ ", deltaReverse="
        ++ show deltaReverse
        ++ ", continuingValue="
        ++ show continuingValue
    )
  pure continuingOut
 where
  TwoWayOrderInfo {twoiUTxOValue = currentValue} = twoi
  expectedDeposit = toInteger twocd.twocdMinDeposit
  currentLovelace = amountOf currentValue GYLovelace

  BPgeniusyield_dex_v2_types_order_OrderDatum0OrderDatum owner paymentAddr nftName aDetails bDetails price validityStart validityEnd takerFlat takerRatio makerRatio freshness = orderDatum
  BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails aAssetClass _ = aDetails
  BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails bAssetClass _ = bDetails

  updatedDatum =
    BPgeniusyield_dex_v2_types_order_OrderDatum0OrderDatum
      owner
      paymentAddr
      nftName
      (BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails aAssetClass newStraight)
      (BPgeniusyield_dex_v2_types_order_AssetDetails0AssetDetails bAssetClass newReverse)
      price
      validityStart
      validityEnd
      takerFlat
      takerRatio
      makerRatio
      freshness

  continuingDatum = datumFromPlutusData updatedDatum

  deltaStraight = newStraight - currentStraight
  deltaReverse = newReverse - currentReverse

decodeOrderDatum
  :: (GYApiMonad m, HasCallStack)
  => TwoWayOrderInfo
  -> m BPgeniusyield_dex_v2_types_order_OrderDatum
decodeOrderDatum TwoWayOrderInfo {..} =
  case PlutusTx.fromBuiltinData (datumToPlutus' twoiRawDatum) of
    Just d -> pure d
    Nothing -> throwAppError $ someBackendError "invalid two-way order datum"

amountOf :: GYValue -> GYAssetClass -> Integer
amountOf v ac =
  case assetClassToPlutus ac of
    Ledger.AssetClass (cs, tn) -> Ledger.valueOf (valueToPlutus v) cs tn

resolveContinuingDeposit :: Integer -> Integer -> Either Txt.Text Integer
resolveContinuingDeposit minDeposit currentDeposit
  | currentDeposit < minDeposit =
      Left $
        Txt.pack $
          "continuing lovelace below minimum: required at least "
            <> show minDeposit
            <> ", found "
            <> show currentDeposit
  | otherwise = Right currentDeposit

{-
cancelTwoWayOrder :: GYApiMonad m
  => TWORef -> GYTxOutRef -> m (GYTxSkeleton PlutusV3)
cancelTwoWayOrder twors orderRef = do
  ois <- Map.elems <$> getTwoWayOrdersInfos twors [orderRef]
  cancelTwoWayOrders twors ois
-}
{-
getTwoWayOrdersInfos :: GYTxQueryMonad m
  => TWORef
  -> [GYTxOutRef]
  -> m (Map.Map GYTxOutRef TwoWayOrderInfo)
getTwoWayOrdersInfos = undefined
-}
getTwoWayOrderInfo
  :: forall m
   . GYApiQueryMonad m
  => TWORef
  -> GYTxOutRef
  -> m TwoWayOrderInfo
getTwoWayOrderInfo twors orderRef = do
  utxoWithDatum <- utxoAtTxOutRefWithDatum' orderRef
  -- let utxo = fst utxoWithDatum
  let policyId = twoWayOrderNftPolicyId twors.tworRefNft
  vod <- utxoDatumPureWithOriginalDatum' utxoWithDatum
  makeTwoWayOrderInfo policyId orderRef vod

cancelTwoWayOrders
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> [TwoWayOrderInfo]
  -> m (GYTxSkeleton PlutusV3)
cancelTwoWayOrders twors twois = do
  RefTWOCD (cfgRef :!: twocd) <- fetchTwoWayOrderConfig twors.tworRefNft
  nid <- networkId
  gyLogDebug' "TWO.cancel" $ "fill_hash=" ++ show twocd.twocdFillHash ++ ", cancel_hash=" ++ show twocd.twocdCancelHash
  let
    cancelStakeCredential = GYStakeCredentialByScript twocd.twocdCancelHash
    cancelStakeAddress = stakeAddressFromCredential nid cancelStakeCredential
  gyLogDebug' "TWO.cancel" $ "withdraw_stake_credential=" ++ show (stakeAddressToCredential cancelStakeAddress)
  let
    cancelWithdrawal =
      mustHaveWithdrawal
        GYTxWdrl
          { gyTxWdrlStakeAddress = cancelStakeAddress,
            gyTxWdrlAmount = 0,
            gyTxWdrlWitness =
              GYTxWdrlWitnessScript
                (GYStakeValReference twors.tworCancelRef $ twoWayOrderCancelValidator twors.tworRefNft)
                unitRedeemer
          }
    accumulatedSkeleton =
      foldMap
        ( \twoi@TwoWayOrderInfo {..} ->
            let paymentValue = cancelPayoutValue twoi
             in mustHaveInput
                  (twoWayOrderInfoToIn twors twoi BPgeniusyield_dex_v2_types_order_OrderRedeemer0CancelOrder)
                  <> mustHaveOutput (twoWayOrderInfoToPayment twoi paymentValue)
                  <> mconcat [mustBeSignedBy pkh | GYCredentialByKey pkh <- twoiOwnerCredentials]
                  <> mustMint
                    ( GYMintReference twors.tworMintRef . mintingPolicyToScript $
                        twoWayOrderMintValidator twors.tworRefNft
                    )
                    nothingRedeemer
                    twoiNFT
                    (-1)
        )
        twois
    feeOutput = mempty

  pure $
    cancelWithdrawal
      <> feeOutput
      <> accumulatedSkeleton
      <> mustHaveRefInput cfgRef
      <> mustHaveTxMetadata stampCancel

-- ???: Can TWO be partially filled in current implementation?
cancelPayoutValue :: TwoWayOrderInfo -> GYValue
cancelPayoutValue TwoWayOrderInfo {..} =
  twoiUTxOValue
    `valueMinus` valueSingleton (GYToken twoiNFTCS twoiNFT) 1

{-
-- !!!: add twoiSecondAsset
twoWayOrderPrice :: TwoWayOrderInfo -> (Natural, Natural) -> GYValue
twoWayOrderPrice info (a, b) = valueSingleton info.twoiFirstAsset.twoiaAssetClass . fromIntegral $ priceOf a
  + valueSingleton info.twoiFirstAsset.twoiaAssetClass . fromIntegral $ priceOf b
  where
  offerPrice :: Natural -> Natural
  offerPrice amt = case info.twoiPrice of
    price@TWOIPriceFixed {} -> ceiling $ rationalToGHC price.twoifStraight * toRational amt
    price@TWOIPriceDeltaOracle {} -> ceiling $ rationalToGHC price.twoioStraight * toRational amt
-}

twoWayOrderInfoToIn
  :: TWORef
  -> TwoWayOrderInfo
  -> BPgeniusyield_dex_v2_types_order_OrderRedeemer
  -> GYTxIn PlutusV3
twoWayOrderInfoToIn twors TwoWayOrderInfo {..} oa =
  GYTxIn
    { gyTxInTxOutRef = twoiRef,
      gyTxInWitness =
        GYTxInWitnessScript
          ( GYInReference twors.tworSpendRef $
              validatorToScript $
                twoWayOrderSpendValidator twors.tworRefNft
          )
          (Just twoiRawDatum)
          $ redeemerFromPlutusData oa
    }

twoWayOrderInfoToPayment
  :: TwoWayOrderInfo -> GYValue -> GYTxOut 'PlutusV3
twoWayOrderInfoToPayment TwoWayOrderInfo {..} v =
  let nftDatum = datumFromPlutusData $ tokenNameToPlutus twoiNFT
   in GYTxOut
        { gyTxOutAddress = twoiOwnerAddr,
          gyTxOutValue = v,
          gyTxOutDatum = Just (nftDatum, GYTxOutUseInlineDatum),
          gyTxOutRefS = Nothing
        }

-- Common pre-computation for placing an order
data PlaceCommon = PlaceCommon
  { pcOwnerPkh :: !GYPubKeyHash,
    pcScriptAddr :: !GYAddress,
    pcNftInput :: !(GYTxIn PlutusV3),
    pcPolicy :: !(GYScript PlutusV3),
    pcPolicyId :: !GYMintingPolicyId
  }

throwSpecError
  :: (GYApiMonad m, HasCallStack)
  => Text
  -> String
  -> m a
throwSpecError code msg =
  throwError
    . GYApplicationException
    $ GYApiError code status400 (Txt.pack msg)

validateValidityRange
  :: (GYApiMonad m, HasCallStack)
  => Maybe GYTime
  -> Maybe GYTime
  -> m ()
validateValidityRange start end =
  for_ ((,) <$> start <*> end) $ \(start', end') ->
    when (end' < start') $
      throwSpecError
        "END_EARLIER_THAN_START"
        ("End time is earlier than start. Start time: " ++ show start' ++ ", end time: " ++ show end')

ensureNoExtraLovelace
  :: (GYApiMonad m, HasCallStack)
  => Natural
  -> m ()
ensureNoExtraLovelace extraLov =
  when (extraLov /= 0) $
    throwSpecError
      "EXTRA_MAKER_LOVELACE_UNSUPPORTED"
      ("Additional lovelace set to " ++ show extraLov ++ ", expected 0")

scriptOutput
  :: PlutusTx.ToData datum
  => PlaceCommon
  -> datum
  -> GYValue
  -> GYTxOut 'PlutusV3
scriptOutput PlaceCommon {..} datum value =
  GYTxOut
    { gyTxOutAddress = pcScriptAddr,
      gyTxOutValue = value,
      gyTxOutDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum),
      gyTxOutRefS = Nothing
    }

data OrderAssembly = OrderAssembly
  { oaValue :: !GYValue,
    oaDatum :: !BPgeniusyield_dex_v2_types_order_OrderDatum
  }

assembleOrderSpec
  :: (GYApiMonad m, HasCallStack)
  => PlaceCommon
  -> TwoWayOrderConfigDatumF GYAddress
  -> TWPlaceSpec
  -> GYTokenName
  -> GYValue
  -> GYValue
  -> m OrderAssembly
assembleOrderSpec PlaceCommon {..} twocd TWPlaceSpec {..} nftName nftValue depositValue =
  case twpsDirection of
    TWOneWay (offerAmt, offerAC) priceAC addOff -> do
      when (offerAmt == 0) $
        throwSpecError "NON_POSITIVE_AMOUNT" ("Amount was :  " ++ show offerAmt)
      when (offerAC == priceAC) $
        throwSpecError
          "NOT_DIFFERENT_OFFERED_ASKED_ASSET"
          ("Offered asset is same as asked asset, which is: " ++ show offerAC)
      when (addOff /= 0) $
        throwSpecError
          "EXTRA_MAKER_ASSET_UNSUPPORTED"
          ("Additional offered asset set to " ++ show addOff ++ ", expected 0")

      let
        offerAmt' = toInteger offerAmt
        offerValue = valueSingleton offerAC offerAmt'
        totalValue = offerValue <> nftValue <> depositValue

      datum <- case twpsPriceSpec of
        TWPriceFixed straight _mRev -> do
          when (straight <= 0) $
            throwSpecError "NON_POSITIVE_PRICE" ("Price was:  " ++ show straight)
          pure $ mkTwoWayOrderFixedDatum pcOwnerPkh twpsOwner nftName (offerAC, offerAmt') priceAC straight twpsStart twpsEnd twocd.twocdTakerFeeFlat twocd.twocdTakerFeeRatio twocd.twocdMakerFeeRatio twocd.twocdOracleFreshnessSeconds
        TWPriceRelative ovk (PriceDelta off spr) _mRev ->
          pure $ mkTwoWayOrderDeltaDatumOneWay pcOwnerPkh twpsOwner nftName (offerAC, offerAmt') priceAC ovk off spr twpsStart twpsEnd twocd.twocdTakerFeeFlat twocd.twocdTakerFeeRatio twocd.twocdMakerFeeRatio twocd.twocdOracleFreshnessSeconds

      pure OrderAssembly {oaValue = totalValue, oaDatum = datum}
    TWTwoway (offerAmt, offerAC) (revAmt, revAC) -> do
      when (offerAmt == 0 && revAmt == 0) $
        throwSpecError "NON_POSITIVE_AMOUNT" "Both amounts zero"
      when (offerAC == revAC) $
        throwSpecError
          "NOT_DIFFERENT_OFFERED_ASKED_ASSET"
          ("Assets identical: " ++ show offerAC)

      let
        offerAmt' = toInteger offerAmt
        revAmt' = toInteger revAmt
        baseValue =
          valueSingleton offerAC offerAmt'
            <> valueSingleton revAC revAmt'
            <> nftValue
            <> depositValue

      datum <- case twpsPriceSpec of
        TWPriceFixed straight mRev -> do
          when (straight <= 0) $
            throwSpecError "NON_POSITIVE_PRICE" ("Price was:  " ++ show straight)
          let revP = maybe (recip straight) id mRev
          pure $ mkTwoWayOrderFixedDatumTwoWay pcOwnerPkh twpsOwner nftName (offerAC, offerAmt') (revAC, revAmt') straight revP twpsStart twpsEnd twocd.twocdTakerFeeFlat twocd.twocdTakerFeeRatio twocd.twocdMakerFeeRatio twocd.twocdOracleFreshnessSeconds
        TWPriceRelative ovk (PriceDelta off spr) mRev -> do
          let mRev' = (\(PriceDelta o s) -> (o, s)) <$> mRev
          pure $ mkTwoWayOrderDeltaDatumTwoWay pcOwnerPkh twpsOwner nftName (offerAC, offerAmt') (revAC, revAmt') ovk off spr mRev' twpsStart twpsEnd twocd.twocdTakerFeeFlat twocd.twocdTakerFeeRatio twocd.twocdMakerFeeRatio twocd.twocdOracleFreshnessSeconds

      pure OrderAssembly {oaValue = baseValue, oaDatum = datum}

buildPlaceChunk
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> TwoWayOrderConfigDatumF GYAddress
  -> PlaceCommon
  -> GYTxOutRef
  -> Integer
  -> GYRedeemer
  -> TWPlaceSpec
  -> m (GYTxSkeleton PlutusV3, GYValue)
buildPlaceChunk twors twocd pc@PlaceCommon {..} nftRef idx mintRedeemer spec@TWPlaceSpec {..} = do
  validateValidityRange twpsStart twpsEnd
  ensureNoExtraLovelace twpsAddLov

  let
    makerFeeValue = valueFromLovelace (toInteger twocd.twocdMakerFeeFlat)
    nftName = expectedTwoTokenNameN nftRef idx
    nftToken = GYToken pcPolicyId nftName
    nftValue = valueSingleton nftToken 1
    depositValue = valueFromLovelace (toInteger twocd.twocdMinDeposit)
  OrderAssembly {..} <- assembleOrderSpec pc twocd spec nftName nftValue depositValue
  let
    straightSpecAmount =
      case twpsDirection of
        TWOneWay (offerAmt, _) _ _ -> Just (toInteger offerAmt)
        TWTwoway (offerAmt, _) _ -> Just (toInteger offerAmt)
    reverseSpecAmount =
      case twpsDirection of
        TWTwoway _ (revAmt, _) -> Just (toInteger revAmt)
        _ -> Nothing
  case straightSpecAmount of
    Just amt ->
      gyLogDebug' "TWO.place.spec.straightAmount" (show amt)
    Nothing -> pure ()
  case reverseSpecAmount of
    Just amt ->
      gyLogDebug' "TWO.place.spec.reverseAmount" (show amt)
    Nothing -> pure ()
  gyLogDebug' "TWO.place.orderValue.assets" (show (valueToList oaValue))

  let
    output = scriptOutput pc oaDatum oaValue
    chunkSkeleton =
      mustHaveOutput output
        <> mustMint (GYMintReference twors.tworMintRef $ mintingPolicyToScript pcPolicy) mintRedeemer nftName 1

  pure (chunkSkeleton, makerFeeValue)

-- (single-order convenience path removed)

-- Variant that uses a preselected NFT reference input (for multi-order placement)
buildPlaceCommonWithRef
  :: (GYApiMonad m, HasCallStack)
  => TWORef -> GYAddress -> Maybe GYStakeCredential -> GYTxOutRef -> m PlaceCommon
buildPlaceCommonWithRef TWORef {..} addr stakeCred nftRef = do
  pkh <- addressToPubKeyHash' addr
  outAddr <- twoWayOrderAddr tworRefNft
  nid <- networkId
  outAddr' <- case addressToPaymentCredential outAddr of
    Nothing -> throwError . GYConversionException $ GYNotScriptAddress outAddr
    Just pc -> pure $ addressFromCredential nid pc stakeCred
  let
    nftInput = GYTxIn {gyTxInTxOutRef = nftRef, gyTxInWitness = GYTxInWitnessKey}
    policy = twoWayOrderMintValidator tworRefNft
  pure
    PlaceCommon
      { pcOwnerPkh = pkh,
        pcScriptAddr = outAddr',
        pcNftInput = nftInput,
        pcPolicy = policy,
        pcPolicyId = mintingPolicyId policy
      }

-- placeTwoWayOrderOneWayCore (removed)

--------------------------------------------------------------------------------
-- Master multi-order placement
--------------------------------------------------------------------------------

-- No helper needed: Aiken mint policy supports multi-mint from a SINGLE input ref
-- by prefixing a 1-byte counter [1..n] into each token name. We therefore pick
-- a single wallet UTxO and use it for all NFTs in the batch.

{- | Place one or more TWO orders in a single transaction.
  All specs must share the same owner address (current limitation for reliable NFT input selection).
-}
placeTwoWayOrders
  :: (GYApiMonad m, HasCallStack)
  => TWORef
  -> NonEmpty TWPlaceSpec
  -> GYTxOutRef
  -> TwoWayOrderConfigDatumF GYAddress
  -> m (GYTxSkeleton PlutusV3)
placeTwoWayOrders twors specs cfgRef twocd = do
  -- Basic preconditions and owner unification
  let owner0 = case NE.head specs of TWPlaceSpec {twpsOwner = o} -> o
  when (not $ all (\TWPlaceSpec {twpsOwner = o} -> o == owner0) (NE.toList specs)) $
    throwError $
      GYApplicationException $
        GYApiError "MULTI_OWNER_UNSUPPORTED" status400 (Txt.pack "All TWPlaceSpec owners must be identical for multi-order placement")

  -- Select a single input ref and set the redeemer amount to total orders count
  let k = fromIntegral (NE.length specs) :: Integer
  nftRef <- someUTxOWithoutRefScript
  gyLogDebug' "TWO.place.nftRef" $ show nftRef
  let mStake = case NE.head specs of TWPlaceSpec {twpsStakeCred = s} -> s
  pc@PlaceCommon {..} <- buildPlaceCommonWithRef twors owner0 mStake nftRef
  -- Recompute the mint redeemer with correct multi-mint amount
  let
    V1.TxOutRef (V1.TxId tid) ix = txOutRefToPlutus nftRef
    outRef = BPcardano_transaction_OutputReference0OutputReference tid (fromIntegral ix)
    oriK = BPgeniusyield_dex_v2_types_order_OutputReferenceInt0OutputReferenceInt outRef k
    mintRedeemerK = redeemerFromPlutusData (BPMintRedeemer0Some oriK)

  -- Build each order skeleton using the shared ref; token names vary with index [1..k]
  chunksWithFees <- for (zip [1 ..] (NE.toList specs)) $ \(i :: Integer, spec) ->
    buildPlaceChunk twors twocd pc nftRef i mintRedeemerK spec

  gyLogDebug' "TWO.place" $ "cfgRef=" ++ show cfgRef
  let
    (skeletonChunks, feeValues) = unzip chunksWithFees
    totalFeeValue = mconcat feeValues
    protocolFeeOutput
      | totalFeeValue == mempty = mempty
      | otherwise =
          let feeDatum =
                datumFromPlutusData $
                  mintingPolicyIdToCurrencySymbol $
                    twoWayOrderNftPolicyId twors.tworRefNft
           in mustHaveOutput
                GYTxOut
                  { gyTxOutAddress = twocd.twocdFeeAddr,
                    gyTxOutValue = totalFeeValue,
                    gyTxOutDatum = Just (feeDatum, GYTxOutUseInlineDatum),
                    gyTxOutRefS = Nothing
                  }

    baseSkeleton =
      mustHaveInput pcNftInput
        <> mustHaveRefInput cfgRef
        <> mustHaveTxMetadata stampPlaced

  gyLogDebug' "TWO.place" $ "cfgRef=" ++ show cfgRef
  pure (protocolFeeOutput <> baseSkeleton <> mconcat skeletonChunks)

data TWFillDirection
  = FillDirectionStraight
  | FillDirectionReverse
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

data TWFillSpec = TWFillSpec
  { twfsOrderRef :: !GYTxOutRef,
    twfsDirection :: !TWFillDirection,
    twfsAmount :: !Natural,
    twfsOracleCertificate :: !(Maybe OracleCertificate),
    twfsRecipient :: !GYAddress
  }
  deriving stock (Generic, Show)
