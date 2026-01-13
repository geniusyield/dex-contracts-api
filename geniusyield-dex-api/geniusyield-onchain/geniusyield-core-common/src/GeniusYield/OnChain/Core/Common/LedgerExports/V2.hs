{-# LANGUAGE CPP #-}

module GeniusYield.OnChain.Core.Common.LedgerExports.V2 (
  module LedgerV2,
) where

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V2 as LedgerV2
#else
import Plutus.V2.Ledger.Api as LedgerV2
#endif
