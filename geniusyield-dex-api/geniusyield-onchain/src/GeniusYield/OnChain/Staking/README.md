# Staking

## Table of Content

 - [Introduction](#introduction)
 - [Process](#process)
 - [The Stake Smart Contract](#the-stake-smart-contract)

## Introduction

Seeing as Cardano is a Proof of _Stake_ blockchain, staking of _ada_ is of course an essential part of Cardano,
built into Cardano at such fundamental levels as Ouroboros consensus and the ledger rules.

When we talk about "staking" here, however, we are not talking about this "native" staking of _ada_,
but instead about project specific staking of some other native token(s):

Projects often mint project specific (utility-)tokens and want to encourage their users to _hold_ these tokens
instead of selling them as quickly as possible.
As an incentive for such so-called _staking_, users get _rewarded_ with more tokens.

To be more precise, users put their tokens "somewhere", maybe for a fixed amount of time (like three months or a year),
and during this time, they can't spend these "staked" tokens. This is in contrast to the native staking of ada,
where the owners keep control of their ada at all times and can spend them whenever they please.

Using a tool like [DB Sync](https://github.com/input-output-hk/cardano-db-sync),
the project can check which user staked which tokens for which period of time and can then - following project specific rules -
determine the _rewards_ each "staker" has accumulated.

Stakers can then _withdraw_ their accumulated rewards.

We support this process by providing [stake](Stake.hs) smart contract.

Users send their tokens to this smart contract to stake them.
They can later retrieve them, either whenever they like or after a fixed period of time.
While their tokens "sit" at the address of this contract, the project can easily detect them and record the amount of tokens being staked
and the time period during which they have been residing at that address.

The project then puts rewards at one or more UTxOs at the address of the rewards key (we are yet to document this process),
where users can withdraw their rewards from.

Parts of this will be handled off-chain by the project: The project will scan the blockchain to determine who has staked what for how long,
and the accumulated rewards will be calculated off-chain, following project-specific rules.
However, both the staking and the withdrawal of rewards will be recorded on-chain in the form of transactions,
which will provide _transparancy_ (thus be checked by the community) and _robustness_ (will not be fatally impacted by a crashed database): All relevant information can always be observed, obtained and reconstructed
by looking at on-chain transactions.

## [The Stake Smart Contract](Stake.hs)

This contract has _no_ parameters. Users stake by creating a UTxO at the contract address with a datum of type [`StakeDatum`](Stake/Types.hs):

```haskell
data StakeDatum = StakeDatum
    { sdOwnerKey    :: !PubKeyHash        -- ^ The owner key. To retrieve the funds, the owner must sign.
    , sdOwnerAddr   :: !Address           -- ^ The owner address. This has no impact on validation.
    , sdLockedUntil :: !(Maybe POSIXTime) -- ^ If set, this denotes the earliest time when the funds can be retrieved.
    }
```

containing their _key_, their _address_ and an (optional) _earliest retrieval time_.

The address `sdOwnerAddr` has no significance for validation.
It is there for the benefit of the project, which will use this address to identify users.

The validator is very simple and will allow spending by transactions who satisfy the following two conditions:

 - The transactions is signed by the key `sdOwnerKey`.
 - If `sdLockedUntil` is `Nothing`, there is no further condition.
   If, on the other hand, it is `Just` a time, the transaction's validity interval must not be before that time.
