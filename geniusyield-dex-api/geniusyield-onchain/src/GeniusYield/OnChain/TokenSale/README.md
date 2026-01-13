# Token Sale

## Table of Content

 - [Introduction](#introduction)
 - [Parameters](#parameters)
 - [Process](#process)
 - [Sale Phase Tokens](#sale-phase-tokens)
 - [Orders](#orders)

## Introduction

The purpose of these smart contracts is to facilitate a _token sale_, where a seller (typically a startup) offers tokens
for a fixed price. Buyers can offer to buy a certain number of tokens (in a given range) during the _sale period_,
whereupon the seller distributes tokens to them during the _distribution period_.

There are certain aspects to this process which are impossible (or at least very difficult) to handle on the blockchain:

 - There may be a whitelist of eligible buyers, for examples those that have undergone KYC.
 - Buyers may ask for more tokens than are available, in which case the seller has to decide on how many to sell to whom
   after having inspected all offers.

Until now, most sellers have done token sales "manually": Buyers send them ada and get tokens in return.

This can work if done well, but it requires a large amount of trust from the buyers, who have to send their money to the seller's
wallet, hoping they will receive tokens or at least their money back eventually.

In contrast, we offer a hybrid solution, where buyers do not send money directly to the seller, but to a smart contract instead.
This contract does _not_ guarantees that they will get tokens in return, but it _does_ guarantee the following:

 - If a buyer has not received any tokens when the distribution phase ends (be it because his address is not whitelisted,
   be it for other reasons), he can get all his ada back (minus the usual Cardano transaction fees).
 - If a buyer _does_ receive tokens, he will get at least the minimal allocated amount of them, and he will get all tokens at the agreed upon price.
 - If a buyer receives less tokens than he asked for, he will get the correct change.
 - The platform provider will receive an agreed upon fee for each sold token.

## Parameters

Token sales are parameterized by a value of type [`TokenSaleParams`](Order.hs):

```haskell
data TokenSaleParams = TokenSaleParams
    { tspBeginSale       :: !POSIXTime  -- ^ Start time for buying the token.
    , tspEndSale         :: !POSIXTime  -- ^ Deadline for buying the token.
    , tspEndDistribution :: !POSIXTime  -- ^ Deadline for distributing the bought tokens.
    , tspToken           :: !AssetClass -- ^ The token on sale.
    , tspPrice           :: !Rational   -- ^ Price for one token in lovelace.
    , tspSellerKey       :: !PubKeyHash -- ^ The token seller.
    , tspMinAllocation   :: !Integer    -- ^ The minimal token allocation.
    , tspFee             :: !Rational   -- ^ The fees for the platform provider.
    , tspFeeAddress      :: !Address    -- ^ The address where the fees must be sent to.
    }
```
 - The buyer is supposed to place his order (by sending ada to the [order contract](Order.hs))
   in the time from `tspBeginSale` to `tspEndSale`.
 - <a name="dist">
       The seller has time to accept orders and distribute tokens during the <em>distribution period</em> from <code>tspEndSale</code> until <code>tspEndDistribution</code>.
       After that time has passed, buyers can claim back the money they locked into the contract.
   </a>
 - The token that the sale is all about is given by `tspToken`.
 - One token costs `tspPrice` lovelace.
 - The seller is identified by `tspSellerKey`. This may be the seller himself, but it could also be the platform provider,
   assuming the seller trusts the platform provider with the tokens and the payments.
 - Buyers have to request at least `tspMinAllocation` many tokens.
 - <a name="fees">
       Upon sale of each token, the platform provider will receive a fee, given as a ratio <code>tspFee</code> of the token price in lovelace.
       So for example, if one token costs one ada, if <code>tspFee</code> is 0.01 and if a buyer receives 500 tokens,
       then 5 ada from the money locked in the contract will go to the platform provider.
   </a>
 - Platform fees are sent to `tspFeeAddress`.

## Process

After the seller has decided on the parameters of a given sale, buyers can make offers by locking ada and one special
[sale phase token](SalePhaseToken.hs) into the [order contract](Order.hs)).

Once `tspEndSale` has passed, the seller will scan the blockchain for valid orders.
Some aspects of validity can be checked on the blockchain itself.
For example, as will be explained later, the presence of the [sale phase token](SalePhaseToken.hs)
in an order UTxO will guarantee that the order was placed between `tspBeginSale` and `tspEndSale`.
Other aspects - like whether the buyer has undergone KYC - must be checked "offchain".

Once the seller has identified the valid orders and decided on how many tokens (above the minimal allocation `tspMinAllocation`)
to give to each valid order, he can send tokens and change to the buyers and fees to the platform provider.

After `tspEndDistribution` has been reached, all buyers who have not received any tokens can _cancel_ their order and get all their locked
ada back.

## [Sale Phase Tokens](SalePhaseToken.hs)

As explained above, valid orders must be placed between `tspBeginSale` and `tspEndSale`.
Since smart contract validation only occurs when trying to _spend_ an UTxO, but not when _creating_ one, this can not be checked during validation (of the [order](Order.hs) contract).

We therefore use a trick: We define a minting policy for a token called "sale phase token",
which only allows minting between `tspBeginSale` and `tspEndSale`, and we check for the presence of such a token in the order value.
(The token name will be fixed.)

This by itself would still not be enough: A malicious user could _mint_ the token during the correct time interval, but then still create the order after the deadline.

For this reason - as part of the minting policy - we insist that the freshly minted token is sent to the order address in the minting transaction.

This is _still_ not sufficient: A (very devious) malicious user could mint the token and send it to the order address, then _cancel_ that order, retrieve the token and send it to a _new_ order after the deadline. In order to prevent this, we check in the _order validator_ that an order UTxO can not be spent without burning the token.

We thus ensure that a sale phase token can only ever "live" at the order address, and we thus get the desired guarantee that any order UTxO with the sale phase token present has been created
during the sales interval.

To summarize, the sale phase token minting policy is:

 - Arbitrary _burning_ is allowed with no further conditions.
 - _Minting_ is allowed under the following conditions:

    - Exactly one coin is minted.
    - Only one specified token name is allowed ("GY" in practice).
    - The freshly minted coin is sent to the order address[^1].
    - Minting is only allowed during the sale period.

[^1]: The minting policy needs to know about the order address, and the order validator needs to know about the sale phase token. This is a circular dependency and tricky to get right.
      We solve it by parameterizing th minting policy by the order address and by _not_ parameterizing the order validator by the minting policy.
      The order validator "discovers" the sale phase token by looking for a non-ada coin in its value. It is up to the offchain code to use the correct parameters and then make sure to only
      consider order UTxO's that contain the correct coin.

## [Orders](Order.hs)

UTxO's locked by this validator can be unlocked in two different ways:

 - If the seller decides (for whatever reason) not to send any tokens to the buyer (aka the "owner"), the buyer can _cancel_ the order once distribution has ended and retrieve his ada.
 - The seller can _fill_ an order and sell tokens to the buyer during the [_distribution period_](#dist).
   He has to send at least the _minimal allocation_ amount of tokens, and he has to send tokens (and the correct change)
   at the specified _price_. Furthermore, he must pay the specified _fees_ for each sold token to the platform provider.

In both cases, the  [_sale phase token_](#sale-phase-tokens) has to be burnt.

We have to be careful not to run afoul of the _double satisfaction problem_: The seller will try to batch-process orders and fill as many as possible in a single transaction.
It can be tricky to guarantee that in this case, correct payments are made for each processed order.
We solve this by making use of the fact that _every_ UTxO on the Cardano blockchain can have a datum attached - not just UTxO's at script addresses.
We will attach a reference to the filled order as datum to each payment, thus making payments for different orders distinguishable.

Accordingly, the order validator will check the following conditions:

 - The [_sale phase token_](#sale-phase-token) contained in the order has to be burnt. We check this by making sure that no transaction output contains the token[^2][^3].
 - If the order is _cancelled_, we check:

    - The _owner_ of the order has signed the transaction.
    - The transaction happens after the [_distribution phase_](#dist).

 - If the order is _filled_, the conditions are:

    - The _seller_ has signed the transaction.
    - The transaction happens during the [_distribution phase_](#dist).
    - At least the _minimal allocation_ amount of tokens must be paid to the _owner_.
    - The owner must receive a sufficient amount of tokens and change: The value of the tokens (according to the specified price) _plus_ the change _minus_ the fees
      must not be smaller than the offered amount[^4].
    - The _platform provider_ must receive sufficient [fees](#fees), which are given by the amount of sold tokens, their price and the specified fee.

[^2]: We could try to check the `txInfoMint` field of the transaction info instead, but then we would need to be extremely careful to account for the presence of several orders as inputs to the transaction.

[^3]: We identify the token by scanning the input value for the only non-ada currency symbol. It is the offchain code's responsibility to only consider orders where the correct token is present.

[^4]: Orders will always include an additional _minimal deposit_ (to ensure the Cardano ledger rules for minimal UTxO values are met), which is not taken into account during price- and fee-calculations
      and which will be paid back to the owner.
