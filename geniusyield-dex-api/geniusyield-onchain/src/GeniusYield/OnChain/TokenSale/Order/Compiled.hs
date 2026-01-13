{-# LANGUAGE RecordWildCards #-}

module GeniusYield.OnChain.TokenSale.Order.Compiled (originalOrderValidator, optimizedOrderValidator) where

import Data.Default (def)
import Data.Text (Text)
import GeniusYield.OnChain.TokenSale.Order
import GeniusYield.Plutonomy ()
import Plutarch.Api.V1
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Extra.RationalData
import Plutarch.Prelude
import Plutarch.Unsafe qualified as PUNSAFE
import Plutonomy qualified
import PlutusLedgerApi.V1
import PlutusTx.Ratio qualified as PlutusTx
import Ply (ScriptRole (ValidatorRole), TypedScript)
import Ply.Plutarch

originalOrderValidator
  :: Either
       Text
       ( TypedScript
           'ValidatorRole
           [ POSIXTime,
             POSIXTime,
             CurrencySymbol,
             TokenName,
             PlutusTx.Rational,
             PubKeyHash,
             Integer,
             PlutusTx.Rational,
             Address,
             TokenName
           ]
       )
originalOrderValidator = toTypedScript def mkOrderValidator'

optimizedOrderValidator
  :: Either
       Text
       ( TypedScript
           'ValidatorRole
           '[ POSIXTime,
              POSIXTime,
              CurrencySymbol,
              TokenName,
              PlutusTx.Rational,
              PubKeyHash,
              Integer,
              PlutusTx.Rational,
              Address,
              TokenName
            ]
       )
optimizedOrderValidator = Plutonomy.optimizeUPLC <$> originalOrderValidator

mkOrderValidator'
  :: ClosedTerm
       ( PPOSIXTime
           :--> PPOSIXTime
           :--> PCurrencySymbol
           :--> PTokenName
           :--> PRationalData
           :--> PPubKeyHash
           :--> PInteger
           :--> PRationalData
           :--> PAddress
           :--> PTokenName
           :--> PV2.PValidator
       )
mkOrderValidator' =
  plam $
    \endSaleT
     endDistribT
     csToken
     tnToken
     price
     sellerKey
     minAlloc
     fee
     feeAddr
     tnSPT
     datm
     redm
     ctx ->
        popaque $
          mkOrderValidator
            # endSaleT
            # endDistribT
            # csToken
            # tnToken
            # (prationalFromData # price)
            # sellerKey
            # minAlloc
            # (prationalFromData # fee)
            # feeAddr
            # tnSPT
            # PUNSAFE.punsafeCoerce datm
            # PUNSAFE.punsafeCoerce redm
            # ctx
