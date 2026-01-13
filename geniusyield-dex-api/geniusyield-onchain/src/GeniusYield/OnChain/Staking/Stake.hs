{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}

module GeniusYield.OnChain.Staking.Stake (stakeValidator) where

import GeniusYield.OnChain.Plutarch.Api
import GeniusYield.OnChain.Staking.Stake.Types
import Plutarch.Api.V2
import Plutarch.Prelude

{- | Validates the retrieval of staked funds. Such funds can only be retrieved under the following conditions:

 - The owner (as specified in the datum) has signed the transaction.
 - Retrieval does happen after the specified time if one is provided.
-}
stakeValidator
  :: Term
       s
       ( PAsData PStakeDatum
           :--> PAsData PUnit
           :--> PAsData PScriptContext
           :--> PUnit
       )
stakeValidator = plam $ \sd _ ctx ->
  validator # pfromData sd #$ pfield @"txInfo" #$ pfromData ctx
 where
  validator
    :: Term
         s
         ( PStakeDatum
             :--> PTxInfo
             :--> PUnit
         )
  validator = plam $ \sd info ->
    unTermCont $ do
      signedByOwner <-
        pletC $
          ptxSignedBy # (pfield @"ownerKey" # sd) #$ pfield @"signatories" # info

      lockedDeadline <- pmatchC (pfromData $ pfield @"lockedUntil" # sd)

      pguardC "must be signed by owner" signedByOwner

      case lockedDeadline of
        PDNothing _ -> return (pconstant ())
        PDJust t ->
          return $
            pif
              (pcontains # (pFrom #$ pfield @"_0" # t) #$ pfield @"validRange" # info)
              (pconstant ())
              (ptraceError "too early")
