{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -O2 -fspecialize-aggressively #-}

module GeniusYield.OnChain.DEX.NFT (mkNFTPolicy) where

import           Prelude                          (($), (.))

import           Plutarch.Api.V1
import qualified Plutarch.Api.V1.AssocMap         as PMap
import           Plutarch.Prelude

import           GeniusYield.OnChain.Plutarch.Api
import qualified Plutarch.Api.V2 as PV2

mkNFTPolicy ::
  Term s (PAsData (PMaybeData PTxOutRef)
     :--> PV2.PScriptContext
     :--> PUnit
         )
mkNFTPolicy = plam $ \mtxOutRef ctx -> policy (pfromData mtxOutRef) ctx
  where
    policy ::
          Term s (PMaybeData PTxOutRef)
      ->  Term s PV2.PScriptContext
      ->  Term s PUnit

    policy txOutRef ctx = pmatch txOutRef
      $ \case
           PDNothing _      -> checkIfBurning # (pownSymbol # ctx)
                                              # pfromData (pfield @"txInfo" # ctx)

           PDJust    rec -> plet (pfield @"txInfo" # ctx) $ \info
                             -> plet (pfield @"_0" # rec) $ \ref
                             -> validateMinting #
                                      ref #
                                      (pexpectedTokenName # ref) #
                                      info #
                                      (mintedTokens_ # (pownSymbol # ctx) # info)

    validateMinting ::
        Term s (PTxOutRef
          :-->  PTokenName
          :-->  PV2.PTxInfo
          :-->  PMap any PTokenName PInteger
          :-->  PUnit
               )
    validateMinting = plam $
      \txOutRef tn info ->
          let
             hasUtxoConsumed  = putxoConsumed # txOutRef # (pfield @"inputs" # info)
             mintedTnAmt   x  = pfromData (psndBuiltin # x)
             mintedTnName  x  = pfromData (pfstBuiltin # x)
             errMsg           = "Expected: 1.) UTxO consumption. 2.) Minted Amount should be 1. 3.) Valid Token name."
          in
          pelimList (\h t -> pif
                            (  hasUtxoConsumed
                           #&& pnull # t
                           #&& mintedTnAmt h  #== 1
                           #&& mintedTnName h #== tn
                            )
                            (pconstant ())
                            (ptraceError errMsg)
                          ) (ptraceError "minted tokens list should not be empty.") . pto

    checkIfBurning ::
      Term s (PCurrencySymbol
        :-->  PV2.PTxInfo
        :-->  PUnit
             )
    checkIfBurning = plam $
         \cs info -> pif
                      (pany #
                        plam (\v -> 0 #<= pfromData (psndBuiltin # v)) #
                        pto (mintedTokens_ # cs # info)
                      )
                      (ptraceError "expected only burning")
                      (pconstant ())

mintedTokens_ ::
  Term s (PCurrencySymbol
     :--> PV2.PTxInfo
     :--> PMap 'Sorted PTokenName PInteger
         )
mintedTokens_ = plam $
    \cs info ->
      pmatch (PMap.plookup # cs # pto (pfromData $ pfield @"mint" # info)) $
        \case
          PNothing -> perror
          PJust m  -> m
