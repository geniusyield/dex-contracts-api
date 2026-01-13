{-# LANGUAGE CPP #-}

module GeniusYield.OnChain.Core.Common.LedgerExports.Common (
  module LedgerCommon,
) where

-- These types are shared in both V1 and V2.

#ifdef NEW_LEDGER_NAMESPACE
import           PlutusLedgerApi.V1       as LedgerCommon hiding (ScriptContext (..), TxInInfo (..), TxInfo (..), TxOut)
import           PlutusLedgerApi.V1.Time  as LedgerCommon (DiffMilliSeconds (DiffMilliSeconds))
import           PlutusLedgerApi.V1.Value as LedgerCommon (AssetClass (AssetClass))
#else
import           Plutus.V1.Ledger.Api     as LedgerCommon hiding (ScriptContext (..), TxInInfo (..), TxInfo (..), TxOut)
import           Plutus.V1.Ledger.Scripts as LedgerCommon (ScriptHash)
import           Plutus.V1.Ledger.Time    as LedgerCommon (DiffMilliSeconds (DiffMilliSeconds))
import           Plutus.V1.Ledger.Value   as LedgerCommon (AssetClass (AssetClass))
#endif
