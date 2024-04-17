# DEX

## Table of Content

- [DEX](#dex)
  - [Table of Content](#table-of-content)
  - [Introduction](#introduction)
  - [Audit report](#audit-report)
  - [NFT](#nft)
  - [Special NFT for Partially Fillable Orders](#special-nft-for-partially-fillable-orders)
  - [Partially Fillable Orders](#partially-fillable-orders)

## Introduction

The purpose of the DEX smart contracts is to implement a _DEX_, a _Decentralized Exchange_, i.e. a means for users to swap (Cardano native) tokens,
which include ada and arbitrary custom tokens. This is _Decentralized_ in the sense that no central authority
(like a traditional, centralized exchange) is needed: Users can swap without needing to trust some authority or each other.

Our DEX architecture is very open and extensible.

In total, we have four different smart contracts powering our DEX:

 - [_Partially Fillable Orders_](#partially-fillable-orders). Order that can be filled in several transactions, each only taking part of the offered tokens and paying corresponding asked tokens.
 - [_NFT_](#nft). A simple NFT minting policy which guarantees that any token seen with it's currency symbol is an NFT.
 - [_Special NFT for Partially Fillable Orders_](#special-nft-for-partially-fillable-orders). This besides giving the same guarantees as our simple NFT minting policy, also guarantees that any UTxO seen with this token is a sensible order. Besides that, an NFT token in an order is also useful to track it's "identity" over it's lifetime. Note that there is no mutable state in the eUTxO-model, and one way to simulate it is to pass an NFT from the consumed UTxO to the new one being created (with modified value and/or datum) so as to identify "same" abstract order.
 - [_Fee Configuration_](#fee-configuration). Governs fees charged to maker & taker when creating, filling an order respectively.

## Audit Report

Our DEX smart contracts have been audited by [Anastasia Labs](https://anastasialabs.com/) and report is available [here](./Anastasia_Labs____Genius_Yield_Audit.pdf).

## NFT

  > **📃**
  > Contract available [here](NFT.hs).

The purpose of this minting policy is to guarantee that every token seen with this policy - independent of its token name - is an NFT, i.e. a _non-fungible token_, a token that can only exist at most once.

The minting policy is _not_ parameterized and thus statically known, which makes it easy to use in other contracts that are in need of an NFT.

Once one has convinced oneself that each token with this policy is indeed an NFT, one can simply use this policy whenever an NFT is required.

This is achieved by making use of a redeemer for the minting transaction. The redeemer specifies an UTxO (via its `TxOutRef`) that has to be consumed by the minting transaction. The minting policy checks that the specified UTxO is indeed consumed, that exactly one token is minted (whereas arbitrary _burning_ is allowed) and that the token name of that one token is the _hash_ of the `TxOutRef` of that UTxO.

It also checks that not more than one token is minted with script's own currency symbol in a single transaction.

Since UTxO's are unique and can only exist once, this guarantees that only one token with this minting policy and token name can ever be minted (except, of course, if there is a _hash collision_, but we deem this risk to be negligible).

## Special NFT for Partially Fillable Orders

  > **📃**
  > Contract available [here](./PartialOrderNFT.hs).

  > **ⓘ**
  > Before going over this section, it would be helpful to first understand our ["_Fee Configuration_"](#fee-configuration) contract.

Our aim is to have our order validator consider only those orders which have a token minted by this minting policy where this minting policy would guarantee that any token minted under it would represent a valid order[^1], besides giving guarantees of our [simple NFT minting script](#nft).

This contract is parameterized by:

* Payment credential of validator which we use to insist that output UTxO having this newly minted NFT token, is indeed having an address whose payment credential is this given parameter.
    > **💡**
    > The reason why we consider payment credential instead of complete address is that in general we would like to have order UTxO placed at an address whose payment part is controlled by our validator however staking part would still belong to order creator, as until the order is filled, ada in order still belongs to the creator so it would be nice if it never misses the stake snapshot.
* An address and
* an asset class which we use to identify the desired reference input. Idea is to first mint an NFT token using our [simple NFT minting script](#nft) and then putting it at our [fee configuration](#fee-configuration) contract (whose address is the second parameter) with required datum to be referred by this script.

This contract is roughly the same as our [simple NFT minting script](#nft) with more conditions in case redeemer is not `Nothing` checked by `checkOutput` function in contract, which are:

- We identify the expected output as one having this newly minted token and assert that it is indeed going to our validator contract (using first payment credential parameter as described before).
- We parse the datum of this output UTxO, which does various checks like currency symbol is indeed of correct number of bytes (which is 28), etc.

  > **ⓘ**
  > Following is the datum type of validator contract and it would be helpful to read the mentioned description (given as a comment string) for it's fields:
  > ```haskell
  >   -- | Datum specifying a partial order.
  >   data PartialOrderDatum = PartialOrderDatum
  >       { podOwnerKey              :: PubKeyHash                -- ^ Public key hash of the owner. Order cancellations must be signed by this.
  >       , podOwnerAddr             :: Address                   -- ^ Address of the owner. Payments must be made to this address.
  >       , podOfferedAsset          :: AssetClass                -- ^ The asset being offered.
  >       , podOfferedOriginalAmount :: Integer                   -- ^ Original number of units being offered. Initially, this would be same as `podOfferedAmount`.
  >       , podOfferedAmount         :: Integer                   -- ^ The number of units being offered.
  >       , podAskedAsset            :: AssetClass                -- ^ The asset being asked for as payment.
  >       , podPrice                 :: PlutusTx.Rational         -- ^ The price for one unit of the offered asset.
  >       , podNFT                   :: TokenName                 -- ^ Token name of the NFT identifying this order.
  >       , podStart                 :: Maybe POSIXTime           -- ^ The time when the order can earliest be filled (optional).
  >       , podEnd                   :: Maybe POSIXTime           -- ^ The time when the order can latest be filled (optional).
  >       , podPartialFills          :: Integer                   -- ^ Number of partial fills order has undergone, initially would be 0.
  >       , podMakerLovelaceFlatFee  :: Integer                   -- ^ Flat fee (in lovelace) paid by the maker.
  >       , podTakerLovelaceFlatFee  :: Integer                   -- ^ Flat fee (in lovelace) paid by the taker.
  >       , podContainedFee          :: PartialOrderContainedFee  -- ^ Total fees contained in the order.
  >       , podContainedPayment      :: Integer                   -- ^ Payment (in asked asset) contained in the order.
  >       } deriving (Generic, Show)
  >
  >   -- | Representation of total fees contained in the order.
  >   data PartialOrderContainedFee = PartialOrderContainedFee
  >       { pocfLovelaces     :: Integer  -- ^ Fees explicitly charged in lovelaces, like flat lovelace fee collected from maker and taker(s).
  >       , pocfOfferedTokens :: Integer  -- ^ Fees explicitly collected as percentage of offered tokens from maker.
  >       , pocfAskedTokens   :: Integer  -- ^ Fees explicitly collected as percentage of asked tokens from taker.
  >       } deriving (Generic, Show)
  > ```

- Next we check values mentioned for in datum:
  - `podOfferedAmount` is positive and is set same as `podOfferedOriginalAmount`.
  - `podNFT` is set same as our newly minted NFT's token name.
  - `podPartialFills` is zero.
  - `podPrice` is positive.
  - `podOfferedAsset` is different from `podAskedAsset`.
  - If `podStart` & `podEnd` are both not `Nothing`, then mentioned start time must not be after mentioned end time.
  - `podMakerLovelaceFlatFee` is same as `pocfLovelaces` (of `podContainedPayment`) as initially the contribution is entirely due to flat maker lovelace fee. Note that we match against `pocfLovelaces` and not `pocdMakerFeeFlat` as our off-chain code may put more flat lovelace fee then what is mentioned for by fee configuration script.
  - `pocfLovelaces` is greater then or equal to `pocdMakerFeeFlat`.
  - `pocfOfferedTokens` is greater then or equal to `⌈pocdMakerFeeRatio * podOfferedAmount⌉`, `⌊pocdMakerFeeRatio * podOfferedAmount⌋` for version 1 and 1.1 respectively. We later made it floor from earlier ceiling to allow NFT tokens.
  - `podTakerLovelaceFlatFee` is same as `pocdTakerFee`.
  - `pocfAskedTokens` is zero.
  - `podContainedPayment` is zero.
- And lastly, we see if UTxO's value conforms to what is present for in the datum and have the required deposit. I.e., if `{A: N}` denote a value having an amount `N` of an asset class `A`, then UTxO's value should be greater than or equal to `{MINTED_NFT: 1} + {podOfferedAsset: podOfferedAmount} + {LOVELACE: pocdMinDeposit} + {LOVELACE: pocfLovelaces} + {podOfferedAsset: pocfOfferedTokens}` where `MINTED_NFT` denotes asset class of our newly minted NFT in question and `LOVELACE` denote asset class of lovelace.

  > **ⓘ  What is the need of deposit (`pocdMinDeposit`)?**
  >
  > Every UTxO has minimum ada requirement which is proportional to it's size. As an order undergoes partial fills, it's miniminum ada requirements may increase which would thus be required to be paid for from _taker(s)_. Instead we can have the order creator commit some ada which would then be returned once a complete fill happens or when the order is cancelled.

## Partially Fillable Orders

  > **📃**
  > Contract available [here](./PartialOrder.hs).

The partial order mechanism allows one to lock their offered tokens inside a validator, which
can then be either completely or partially filled/bought by another user.

As described in earlier section, each order contains an NFT minted by our special minting policy to identify it and also to guarantee it's correctness. This simplifies off-chain event listener as it need not do various checks to know whether the UTxO indeed represents a valid order. Besides that, our validator also makes use of this special NFT minting policy to determine what all orders are being filled in this transaction so as to correctly charge taker fee _per transaction_ but more on that later!

Note that in case an order UTxO, doesn't have this special NFT token (`TokenName` is fetched from `podNFT` field of datum and we'll see later how validator come to know about it's `CurrencySymbol`), then validator simply allows for it to be spent without any further checks.

This order validator is parameterized over two arguments:

- An address, which would be the address of our fee configuration validator.

- Asset class of an NFT token.

  Idea is, our order validator need to determine the policy id (currency symbol) of our [special minting policy](#special-nft-for-partially-fillable-orders) contract. But as this policy in turn needs address (specifically, payment credential) of this partial order validator contract as a parameter, we find ourselves in chicken-egg situation.
  
  However, we do a trick where we first mint a simple NFT token, using our [simple NFT contract](#nft) contract, then we mention this newly minted token's asset class as parameter to this contract. Now once we have address of our order validator, we give it's payment credential to our [special NFT minting policy](#special-nft-for-partially-fillable-orders). Thus now we have minting policy id of our special NFT minting script.

  As a last step, we create an UTxO at our fee configuration validator (the first parameter to validator), with our initially minted NFT token (whose asset class is second parameter to validator) and with an inline datum specifying, among other fields, the minting policy id of our special NFT minting script.

  Note that even though our fee configuration validator allows spending of an UTxO belonging to it, but it does so under restrictions and one of the restriction is the required continuing output's datum does not mention different minting policy id. Thus, it's not possible for us to mention different minting policy id at a later point. Therefore, now we have solved for our chicken-egg problem. Validator would see whether there is any reference input belonging to address of first parameter, having a unit token with asset class as in second parameter and if so, it's inline datum would give the needed currency symbol of our special NFT minting script.
  
  > **💡**
  > As a summary, the fact that we use an NFT token (**simple** NFT token!) in this reference input, means that there can only be one such reference input and given the restrictions of fee configuration validator, means we can't later spend this UTxO and generate a new one with different minting policy id in it's datum. Note that we could have instead put it at one-way address (which is a script that doesn't allow any UTxO to be spent from it) but have decided to club it with fee configuration script so as to not require an additional reference input in the transaction.

Follow three interactions are possible with this validator:

1. _Cancel_ - The order is cancelled, allowing the UTxO with the remaining offer amount to be reclaimed.

    Requirements:
      - Must be signed by the owner of the order, identified by `podOwnerKey`.
      - The NFT identifying the order must be burnt, lest it escapes into another UTxO.
      - All the accumulated payment, along with initial ada deposit is paid to address given by `podOwnerAddr`.
        - We put in the datum of this payment output, reference to the current UTxO being spent so as to not run afoul of double satisfaction problem.
      - If order underwent zero partial fills (i.e., `podPartialFills` is zero) or contained fee is zero (i.e., `podContainedFee` is `mempty`) then we do not collect allocated maker fees otherwise there has to be a fee output.
        - How do we know which output is a fee output? We find the first output going to the fee address, where this address is obtained from `pocdFeeAddr`.
        - Datum of this fee output has type:
          > ```haskell
          > -- | Datum of the fee output.
          > data PartialOrderFeeOutput = PartialOrderFeeOutput
          >     { pofdMentionedFees :: PlutusTx.Map TxOutRef Value
          >       -- ^ Map, mapping order being consumed to the collected fees.
          >     , pofdReservedValue :: Value
          >       -- ^ Value reserved in this UTxO which is not to be considered as fees.
          >     , pofdSpentUTxORef  :: Maybe TxOutRef
          >       -- ^ If not @Nothing@, it mentions the UTxO being consumed, whose value is used to provide for UTxOs minimum ada requirement.
          >     } deriving (Generic, Show)
          > ```
          Every UTxO, requires some minimum ada, proportional to it's size to be committed to it, thus total fee collected must have ada greater than or equal to this minimum. In future, our fee validator may help us provide an UTxO, with sufficient ada to which we can piggy back fees upon, in which case, collected fees need not satisfy minimum ada requirement. In such a case, this fee validator would check whether the continuing output has `pofdReservedValue` set as original value present and `pofdSpentUTxORef` contains `TxOutRef` of it's consumed UTxO, to prevent double satisfaction.

          `pofdMentionedFees` is main field of interest for our order validator. Our approach towards fee output is to charge fees _per_ transaction. So, if in a single transaction, there are multiple fills (partial or complete), we'll charge single taker fee.

          `pofdMentionedFees` contains a map of an order's `TxOutRef` to it's _"mentioned fees"_, essentially, it should be value corresponding to `podContainedFee` of that order. We can't simply put it as `PlutusTx.Map TxOutRef PartialOrderContainedFee` (instead of `PlutusTx.Map TxOutRef Value`) because different orders might have different offered/asked asset classes.

          Sum of values inside `pofdMentionedFees` fees map represent _"expected contained fees"_ that the off-chain code has put but our order validator would verify that corresponding value mentioned for it's `TxOutRef` is indeed greater than or equal to value corresponding to it's `podContainedFee`.

        - For the cancel case, since there are no taker fees, we would require that the actual value present in this fee output (minus the `pofdReservedValue`) is greater than or equal to the "expected contained fees".

          Also, instead of checking whether "mentioned fees" is greater than or equal to value corresponding to `podContainedFee`, we would rather use it's updated version which would have `pocfOfferedTokens` subtracted by `⌊pocfOfferedTokens * (podOfferedAmount / podOfferedOriginalAmount)⌋`. As we don't want the non-flat maker fee for offered tokens that have not been consumed.

          > **ⓘ**
          >
          > For brevity, let `a`, `b`, `m` denote `podOfferedAmount`, `podOfferedOriginalAmount` and `pocdMakerFeeRatio` respectively.
          >
          > If `F` denote the percentage maker fee that was initially charged (when order was created), then it is `⌈m * b⌉` and `⌊m * b⌋` for version 1 and version 1.1 family of DEX contracts respectively. Since the amount of order which actually got filled was `b - a`, percent maker fee that we actually want to be charged is `⌈m * (b - a)⌉` for version 1 and `⌊m * (b - a)⌋` for version 1.1 respectively. Which however isn't necessarily same as what is being checked by `PartialOrder.hs` contract, namely `F - ⌊(a / b) * F⌋` but the difference is at most 1 as following proofs show.
          >
          > For version 1, we want to show `⌈m * b⌉ - ⌊(a / b) * ⌈m * b⌉⌋ <= ⌈m * (b - a)⌉ + 1`.
          >
          > Working on left hand side, we find,
          >
          > `⌈m * b⌉ - ⌊(a / b) * ⌈m * b⌉⌋`
          >
          > `<= ⌈m * b⌉ - ⌊(a / b) * m * b⌋`
          >
          > `<= ⌈m * b⌉ - ⌊m * a⌋`
          >
          > `== ⌈m * b⌉ + ⌈-m * a⌉` since [negating the argument switches floor and ceiling and changes the sign](https://en.wikipedia.org/wiki/Floor_and_ceiling_functions)
          >
          > `== (⌈m * b⌉ + ⌈-m * a⌉ -1) + 1`
          >
          > `<= ⌈m * (b - a)⌉ + 1` since `⌈x⌉ + ⌈y⌉ - 1 <= ⌈x + y⌉` Q.E.D.
          >
          > And for version 1.1, we want to show `⌊m * b⌋ - ⌊(a / b) * ⌊m * b⌋⌋ <= ⌊m * (b - a)⌋ + 1`.
          >
          > `⌊m * b⌋ = m * b - d` for a suitable `0 <= d < 1` ...(1)
          >
          > `⌊(a / b) * ⌊m * b⌋⌋ = ⌊(a / b) * (m * b - d)⌋` because of (1) and further simplifies to `⌊m * a - a * d / b⌋ = m * a - a * d / b - e` for a suitable `0 <= e < 1` ...(2)
          >
          > `⌊m * (b - a)⌋ = m * (b - a) - f` for a suitable `0 <= f < 1` ...(3)
          >
          > `⌊m * b⌋ - ⌊(a / b) * ⌊m * b⌋⌋ = (m * b - d) - (m * a - a * d / b - e)` (because of (1) and (2))
          >
          > `= m * (b - a) + d * (-1 + a / b) + e = m * (b - a) - f + f + d * (-1 + a / b) + e = ⌊m * (b - a)⌋ + f + d * (-1 + a / b) + e` (because of (3)) ...(4)
          >
          > The left hand side of (4) is an integer, so the right hand side must also be an integer. `⌊m * (b - a)⌋` is also an integer, so this implies that `f + d * (-1 + a / b) + e` must be an integer as well.
          >
          > Since `0 <= d < 1`, that means `-1 + a / b < d * (-1 + a / b) <= 0` (Note that `0 < a < b` and so `0 < a / b < 1`). This along with `0 <= e, f < 1` gives us `-1 + a / b < d * (-1 + a / b) + e + f < 2`. And since `-1 < -1 + a / b < 0`, we have `d * (-1 + a / b) + e + f` to be either `0` or `1`.
          >
          > Q.E.D.

2. _Complete fill_ - The full offered amount is bought.

    Requirements:
      - The NFT identifying the order must be burnt, lest it escapes into another UTxO.
      - Transaction validity interval must be within order validity duration (defined by `podStart` & `podEnd`).
      - All the previous accumulated payment, along with additional payment & initial ada deposit is paid to address given by `podOwnerAddr`.
        - Additional payment (of the asked-for asset) must be greater than or equal to `⌈podOfferedAmount * podPrice⌉`.
        - We put in the datum of this payment output, reference to the current UTxO being spent so as to not run afoul of double satisfaction problem.
      - If the fee corresponding to this order is zero, i.e., value corresponding to `podContainedFee` along with value corresponding to `podTakerLovelaceFlatFee` is `mempty` then a fee output is not insisted for by this order. Otherwise, it does require a fee output, where actual value in fee output (minus the `pofdReservedValue`) must be greater than or equal to sum of "expected contained fees" and flat taker fees (obtained using `podTakerLovelaceFlatFee`). Also, in this fee output, the "mentioned fees" for this order must be greater than or equal to value corresponding to `podContainedFee`. Note that if we don't find an entry in this map for our own `TxOutRef` then `podContainedFee` must be checked to be `mempty`.

3. _Partial fill_ - a portion of the offered amount is bought, by paying corresponding asked tokens.

    Requirements:
      - The portion/amount must be positive, but less than the remaining total amount. Let's call it `amt`.
      - Transaction validity interval must be within order validity duration (defined by `podStart` & `podEnd`).
      - There must be (single) continuing output, where consumed offered tokens are deducted but the additional payment of `⌈amt * podPrice⌉` in asked tokens is added. Next we need to know whether taker fee should be present in this continuing output or not, for that we have two cases:
        * There is no fee output. In this case, we would check whether our continuing output is first "continuing partial fill output" in a transaction or not. If our output is the first one, we'll add flat taker fee to it and also add `podTakerLovelaceFlatFee` to `pocfLovelaces`.
        * There is a fee output. In this case, we'll not require added taker fee for our continuing output and would just check if the value in fee output (minus the `pofdReservedValue`) is greater than or equal to sum of "expected contained fees" and flat taker fee.
      - Note that one may put additional ada and additional tokens of `podAskedAsset` then what is required to spend this UTxO. In such a case continuing output's datum fields corresponding to it, namely, `pocfLovelaces` and `pocfAskedTokens` must be suitably updated.
      - Continuing output datum must match with expectations, which are:
        * `podOfferedAmount` is deducted by `amt`.
        * `podPartialFills` being incremented by 1.
        * `podContainedFee` is updated as described earlier.
        * `podContainedPayment` is increased by `⌈amt * podPrice⌉`.
        * Rest of the fields are same as before.

## Fee Configuration

  > **📃**
  > Contract available [here](./PartialOrderConfig.hs).

For our DEX, we want fees to be _enforced_ by our smart contracts and not just be at the liberty of off-chain code. Secondly, we want the option to _change_ fees without that change resulting in a different script and a different smart contract address.

Note that change in flat taker fees, only apply to _new_ orders and therefore with respect to end users, update to these fee parameters does not affect old orders.

Contract is parameterized by `AssetClass` of an NFT token (which we mint using our [simple NFT minting policy](#nft)) and uses it to require that UTxO being spent contains this token and also to identify continuing output.

Following is the datum type of validator contract and it would be helpful to read the mentioned description (given as a comment string) for it's fields:

```haskell
data PartialOrderConfigDatum = PartialOrderConfigDatum
    { pocdSignatories    :: [PubKeyHash]   -- ^ Public key hashes of the potential signatories.
    , pocdReqSignatories :: Integer        -- ^ Number of required signatures.
    , pocdNftSymbol      :: CurrencySymbol -- ^ Currency symbol of the partial order Nft.
    , pocdFeeAddr        :: Address        -- ^ Address to which fees are paid.
    , pocdMakerFeeFlat   :: Integer        -- ^ Flat fee (in lovelace) paid by the maker.
    , pocdMakerFeeRatio  :: Rational       -- ^ Proportional fee (in the offered token) paid by the maker.
    , pocdTakerFee       :: Integer        -- ^ Flat fee (in lovelace) paid by the taker.
    , pocdMinDeposit     :: Integer        -- ^ Minimum required deposit (in lovelace).
    } deriving (Generic, P.Show)
```

And there is only one redeemer action of type unit `()`.

Idea is to create an UTxO at our validator containing the mentioned NFT value and with desired datum. Then our other DEX contracts, such as [Partially Fillable Orders](#partially-fillable-orders) contract would use this UTxO as a reference input to know for some of the needed datum fields.

Rules governing how this UTxO can be updated:

* As mentioned before, UTxO being spent, must have unit token of the asset class given as parameter for further checks to be exercised, else we allow such an irrelevant UTxO to be spent.
* Transaction has at least `pocdReqSignatories` number of signatures coming from `pocdSignatories` signatories.
* In continuing output, we require that:
  * `pocdSignatories` are all unique and within 1 & 10 (inclusive).
  * `pocdReqSignatories` is positive and less than length of `pocdSignatories`.
  * `pocdNftSymbol` remains same. This field holds the currency symbol of ["Special NFT for Partially Fillable Orders"](#special-nft-for-partially-fillable-orders) contract. Reason why it is stored is explained for in [Partially Fillable Orders](#partially-fillable-orders) contract.
  * There is an output being made in a transaction to the new `pocdFeeAddr`, this guarantees that this address is valid.
  * `pocdMakerFeeFlat`, `pocdTakerFee`, `pocdMinDeposit` are all non negative. Having them as negative would also violate some of the invariants assumed by ["Special NFT for Partially Fillable Orders"](#special-nft-for-partially-fillable-orders) script. Also, we require them to be less than 1000 ADA so as to bound our datum.
  * `pocdMakerFeeRatio` is b/w 0 & 1 (inclusive).


[^1]: This also simplifies our off-chain event listener as it can just look for token minted by our policy script to know for valid order instead of performing various checks against the order UTxO.
