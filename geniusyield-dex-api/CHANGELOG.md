# Revision history for geniusyield-dex-api

## 0.5.1.0 -- 2024-02-14

* Update to Atlas v0.11.1.

## 0.5.0.0 -- 2024-08-30

* Update to Atlas v0.6.0.

## 0.4.0.0 -- 2024-06-27

* Updates definition of `fillMultiplePartialOrders'` to call `buildWithFeeOutput` even in case more than one order is being filled.

## 0.3.1.0 -- 2024-06-06

* Adds a fix to earlier `0.3.0.0`, to actually consider `poiRawDatum` as witness.

## 0.3.0.0 -- 2024-06-05

* Updates to make use of latest Atlas commit, `1c20f2a65de8e087b495d1f3ad524d6e659167ad`.
* Uses original UTxO's datum to provide for witness to prevent for round-trip issues.

## 0.2.1.0 -- 2024-05-07

* Adds `placePartialOrder''`, `placePartialOrderWithVersion''` to also return for order's NFT token.
* Exports `placePartialOrder''`, `placePartialOrderWithVersion`, `placePartialOrderWithVersion'` and `placePartialOrderWithVersion''`.

## 0.2.0.0 -- 2024-04-17

* Adds support for v1.1 family of scripts.
* Adds CIP-20 metadata messages on transactions.

## 0.1.0.0 -- 2023-12-22

* First version.
