# GeniusYield DEX Contracts API

This repository hosts off-chain code to interact with DEX smart contracts.

Main file of interest is [`PartialOrder.hs`](./src/GeniusYield/Api/Dex/PartialOrder.hs).

## Order creation

https://github.com/geniusyield/dex-contracts-api/blob/cdc81e96ee45411786fa160bab51eff1bc281316/src/GeniusYield/Api/Dex/PartialOrder.hs#L429-L542

```haskell
placePartialOrder
  ∷ GYDexApiMonad m a
  ⇒ PORefs
  → GYAddress
  -- ^ Order owner
  → (Natural, GYAssetClass)
  -- ^ Amount and asset to offer.
  → GYAssetClass
  -- ^ The asset being asked for as payment.
  → GYRational
  -- ^ The price for one unit of the offered asset.
  → Maybe GYTime
  -- ^ The earliest time when the order can be filled (optional).
  → Maybe GYTime
  -- ^ The latest time when the order can be filled (optional).
  → Maybe GYStakeCredential
  -- ^ Stake credential of user. We do not support pointer reference.
  → m (GYTxSkeleton 'PlutusV2)
placePartialOrder por@PORefs {..} addr (offerAmt, offerAC) priceAC price start end stakeCred = do
  (cfgRef, pocd) ← fetchPartialOrderConfig porRefNft
  placePartialOrder' por addr (offerAmt, offerAC) priceAC price start end 0 0 stakeCred cfgRef pocd

placePartialOrder'
  ∷ (GYDexApiMonad m a, HasCallStack)
  ⇒ PORefs
  → GYAddress
  -- ^ Order owner
  → (Natural, GYAssetClass)
  -- ^ Amount and asset to offer.
  → GYAssetClass
  -- ^ The asset being asked for as payment.
  → GYRational
  -- ^ The price for one unit of the offered asset.
  → Maybe GYTime
  -- ^ The earliest time when the order can be filled (optional).
  → Maybe GYTime
  -- ^ The latest time when the order can be filled (optional).
  → Natural
  -- ^ Additional lovelace fee.
  → Natural
  -- ^ Additional fee in offered tokens.
  → Maybe GYStakeCredential
  -- ^ Stake credential of user. We do not support pointer reference.
  → GYTxOutRef
  → PartialOrderConfigInfoF GYAddress
  → m (GYTxSkeleton 'PlutusV2)
placePartialOrder' por@PORefs {..} addr (offerAmt, offerAC) priceAC price start end addLov addOff stakeCred cfgRef pocd = do
  when (offerAmt == 0) $ throwAppError $ PodNonPositiveAmount $ toInteger offerAmt
  when (price <= 0) $ throwAppError $ PodNonPositivePrice price
  when (offerAC == priceAC) $ throwAppError $ PodNonDifferentAssets offerAC

  case (start, end) of
    (Just start', Just end') → when (end' < start') $ throwAppError $ PodEndEarlierThanStart start' end'
    _ → pure ()

  pkh ← addressToPubKeyHash' addr
  outAddr ← partialOrderAddr por
  nid ← networkId
  let outAddr' = addressFromCredential nid (addressToPaymentCredential outAddr & fromJust) stakeCred
  policy ← partialOrderNftPolicy por
  nftRef ← someUTxOWithoutRefScript

  let nftName = gyExpectedTokenName nftRef
      nftRedeemer = mkNftRedeemer $ Just nftRef
      nft = GYToken (mintingPolicyId policy) nftName
      nftInput =
        GYTxIn
          { gyTxInTxOutRef = nftRef,
            gyTxInWitness = GYTxInWitnessKey
          }
      nftV = valueSingleton nft 1
      offerAmt' = toInteger offerAmt
      makerFeeFlat = fromIntegral addLov + pociMakerFeeFlat pocd
      makerFeeOff = (+) (fromIntegral addOff) $ ceiling $ toRational offerAmt * rationalToGHC (pociMakerFeeRatio pocd)
      makerFee =
        valueFromLovelace makerFeeFlat
          <> valueSingleton offerAC makerFeeOff
      offerV =
        valueSingleton offerAC offerAmt'
          <> nftV
          <> valueFromLovelace (toInteger $ pociMinDeposit pocd)
          <> makerFee
      containedFee =
        PartialOrderContainedFee
          { pocfLovelaces = makerFeeFlat,
            pocfOfferedTokens = makerFeeOff,
            pocfAskedTokens = 0
          }
      od =
        PartialOrderDatum
          { podOwnerKey = pubKeyHashToPlutus pkh,
            podOwnerAddr = addressToPlutus addr,
            podOfferedAsset = assetClassToPlutus offerAC,
            podOfferedOriginalAmount = offerAmt',
            podOfferedAmount = offerAmt',
            podAskedAsset = assetClassToPlutus priceAC,
            podPrice = rationalToPlutus price,
            podNFT = tokenNameToPlutus nftName,
            podStart = timeToPlutus <$> start,
            podEnd = timeToPlutus <$> end,
            podPartialFills = 0,
            podMakerLovelaceFlatFee = makerFeeFlat,
            podTakerLovelaceFlatFee = pociTakerFee pocd,
            podContainedFee = containedFee,
            podContainedPayment = 0
          }

      o = mkGYTxOut outAddr' offerV (datumFromPlutusData od)

  return
    $ mustHaveInput nftInput
    <> mustHaveOutput o
    <> mustMint (GYMintReference porMintRef $ mintingPolicyToScript policy) nftRedeemer nftName 1
    <> mustHaveRefInput cfgRef
```
