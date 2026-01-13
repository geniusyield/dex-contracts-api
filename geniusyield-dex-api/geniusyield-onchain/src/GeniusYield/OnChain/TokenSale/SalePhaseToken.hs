{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O2 -fspecialize-aggressively -Wno-incomplete-patterns #-}

module GeniusYield.OnChain.TokenSale.SalePhaseToken (
  -- * Plutarch types
  (:-->),
  Term,
  PTokenName (..),
  PPOSIXTime,
  PAddress (..),
  PAsData,
  PUnit (..),
  PScriptContext (..),

  -- * Sale Phase Token minting policy
  mkSalePhaseTokenPolicy,
) where

import GeniusYield.OnChain.Plutarch.Api
import Plutarch.Api.V1
import Plutarch.Api.V1.Value
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Prelude

{- | This defines the minting policy of the Sales Phase Token. It is parameterised over:

    - The 'PTokenName' to allow minting for.
    - The 'PPOSIXTime' when minting becomes possible (which will be `GeniusYield.OnChain.TokenSale.Order.tspBeginSale` in practice).
    - The 'PPOSIXTime' after which minting is no longer allowed (which will be `GeniusYield.OnChain.TokenSale.Order.tspEndSale` in practice).
    - The 'PAddress' where a freshly minted token must be sent to (which will be the address of the order validator in practice).

    The minting policy has the following logic:

    1. Arbitrary burning is allowed.
    2. For minting the following conditions are required:

        - The minting of the token is happening during the sale, given by the two 'PPOSIXTime' arguments.
        - The amount of tokens to be minted is exactly one.
        - The 'PTokenName' must be the specified one.
        - The minted token goes to the specified 'PAddress'.
-}
mkSalePhaseTokenPolicy
  :: Term
       s
       ( PTokenName
           :--> PPOSIXTime
           :--> PPOSIXTime
           :--> PAddress
           :--> PAsData PUnit
           :--> PV2.PScriptContext
           :--> PUnit
       )
mkSalePhaseTokenPolicy = plam $ \tn startT endT addr _ ctx ->
  policy
    # (pownSymbol # ctx)
    # tn
    # startT
    # endT
    # addr
    # (pfield @"txInfo" # ctx)
 where
  policy
    :: Term
         s
         ( PCurrencySymbol
             :--> PTokenName
             :--> PPOSIXTime
             :--> PPOSIXTime
             :--> PAddress
             :--> PV2.PTxInfo
             :--> PUnit
         )
  policy = plam $ \cs tn startT endT addr info ->
    plet (pmintedTokens # cs # tn # info) $ \amt ->
      let
        utxo = sentTo # cs # tn # info

        -- Checks

        checkIfBurning = amt #< 0

        validAmt = amt #== 1
        validUtxo = pdata addr #== pfield @"address" # utxo
        validTimeRange = duringSale # startT # endT # info

        -- Trace error messages

        invalidAmtErr = ptraceError "amount not one."
        invalidUtxoErr = ptraceError "token not sent to address."
        invalidTimeRangeErr = ptraceError "not during sale."

        -- Validate check of error.

        checkValidTimeRange = pif validTimeRange (pconstant True) invalidTimeRangeErr
        checkValidUtxo = pif validUtxo checkValidTimeRange invalidUtxoErr
        checkAll = pif validAmt checkValidUtxo invalidAmtErr
       in
        pif
          ( checkIfBurning
              #|| checkAll
          )
          (pconstant ())
          (ptraceError "Expected burning of the token or All other checks to validate.")

  sentTo
    :: Term
         s
         ( PCurrencySymbol
             :--> PTokenName
             :--> PV2.PTxInfo
             :--> PV2.PTxOut
         )
  sentTo = plam $ \cs tn info ->
    precList
      ( \self txOut txOuts ->
          pif
            ((pvalueOf # pfromData (pfield @"value" # txOut) # cs # tn) #== 1)
            txOut
            (self # txOuts)
      )
      (const $ ptraceError "The token must be present in any output UTxO.")
      # (pfield @"outputs" # info)

  duringSale
    :: Term
         s
         ( PPOSIXTime
             :--> PPOSIXTime
             :--> PV2.PTxInfo
             :--> PBool
         )
  duringSale = plam $ \startT endT info ->
    pcontains # (pinterval # startT # endT) # (pfield @"validRange" # info)
