# Revision history for geniusyield-server-lib

## 0.5.1 -- 2024-06-06

* Incorporates 0.3.1.0 of `geniusyield-dex-api`.

## 0.5.0 -- 2024-06-05

* Makes use of latest `geniusyield-dex-api` version, namely, v0.3.0.0. It includes a fix where original UTxO's datum bytes are used to provide for witness.

## 0.4.1 -- 2024-05-28

* Fix `/v0/orders/fill` endpoint to account for case when percent taker fees is zero.

## 0.4.0 -- 2024-05-20

* Fix response of GET `/v0/orders/details/{nft-token}` endpoint to not return response under a `data` field nesting.
* Added `/v0/orders/fill` endpoint.

## 0.3.0 -- 2024-05-07

* Adds TapTools OHLCV endpoint.
* Adds NFT token in response of place order family of endpoints.
* Adds GET variant for getting details of an order from it's NFT token identifier.
* Clarifies which endpoints require `maestroToken` field to be set.
* Clarifies which endpoints require signing key to be configured in the server to derive for wallet's address, likewise it is clarified that which endpoints use fields such as `collateral`, etc. from server's configuration.

## 0.2.0 -- 2024-04-22

* Uses latest version of `geniusyield-dex-api` which adds support of v1.1 script.
* `settings` endpoint now returns `genius-server` instead of `mmb` as return value of `backend` field.

## 0.1.0 -- 2024-04-02

* First version.
