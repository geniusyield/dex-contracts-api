# GeniusYield DEX Contracts API

This repository hosts off-chain code to interact with DEX smart contracts.

Main file of interest is [`PartialOrder.hs`](./src/GeniusYield/Api/Dex/PartialOrder.hs) and provides various useful API functions among those related to interacting with order's contract.

Order's contract offers three interaction for an existing order, namely:

* Completely filling it.
* Only partially filling it with a specified amount.
* Cancelling it.

These are represented in redeemer as:

https://github.com/geniusyield/dex-contracts-api/blob/8add6b608235095fa019fb6566d8ef1cd81080bf/src/GeniusYield/Scripts/Dex/PartialOrder.hs#L112-L116

And following is the specification of datum:

https://github.com/geniusyield/dex-contracts-api/blob/8add6b608235095fa019fb6566d8ef1cd81080bf/src/GeniusYield/Scripts/Dex/PartialOrder.hs#L75-L108

Where `PartialOrderContainedFee` is defined to be:

https://github.com/geniusyield/dex-contracts-api/blob/8add6b608235095fa019fb6566d8ef1cd81080bf/src/GeniusYield/Scripts/Dex/PartialOrder.hs#L39-L48

## Order creation

Order can be created as described in the following snippet:

https://github.com/geniusyield/dex-contracts-api/blob/cdc81e96ee45411786fa160bab51eff1bc281316/src/GeniusYield/Api/Dex/PartialOrder.hs#L429-L542

## Order fill

And following describes how an existing order can be filled for both the cases, namely partial & complete.

https://github.com/geniusyield/dex-contracts-api/blob/cdc81e96ee45411786fa160bab51eff1bc281316/src/GeniusYield/Api/Dex/PartialOrder.hs#L544-L693

## Order cancellation

Lastly, existing order can be canceled by it's owner, as described in linked snippet:

https://github.com/geniusyield/dex-contracts-api/blob/cdc81e96ee45411786fa160bab51eff1bc281316/src/GeniusYield/Api/Dex/PartialOrder.hs#L695-L751
