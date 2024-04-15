{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module GeniusYield.OnChain.Plutarch.Time ( pcontains
                                         , plowerbound
                                         , pupperBound
                                         , pinterval
                                         , pFrom
                                         , pTo
                                         ) where

import           Plutarch.Api.V1
import           Plutarch.Extra.TermCont (pletFieldsC)
import           Plutarch.Prelude

{- | 'pcontains' is the plutarch level function that is similar to 'contains' defined in "Ledger".
     The function checks whether the second interval 'PPOSIXTimeRange' is completely contained
     withing first 'PPOSIXTimeRange'.
-}
pcontains ::
  Term s (PPOSIXTimeRange
    :-->  PPOSIXTimeRange
    :-->  PBool
         )
pcontains = phoistAcyclic $ plam $ \interval1 interval2
  -> unTermCont $ do
      v1 <- pletFieldsC @'["from", "to"] interval1
      v2 <- pletFieldsC @'["from", "to"] interval2

      let lb1 = pfromData $ getField @"from" v1
          lb2 = pfromData $ getField @"from" v2

          ub1 = pfromData $ getField @"to" v1
          ub2 = pfromData $ getField @"to" v2

      return ( lb1 #<= lb2
          #&&  ub2 #<= ub1
             )


-- | 'plowerbound' is plutarch level function of 'lowerBound'.
plowerbound :: Term s (PPOSIXTime :--> PLowerBound PPOSIXTime)
plowerbound = phoistAcyclic $ plam $ \a
  -> let
        lbValue = pcon $ PFinite (pdcons @"_0" # pdata a # pdnil)
        lb      = PLowerBound (pdcons @"_0" # pdata lbValue #$
                               pdcons @"_1" # pdata (pconstant True) # pdnil
                              )
     in
     pcon lb


-- | 'pupperBound' is plutarch level function of 'upperBound'.
pupperBound :: Term s (PPOSIXTime :--> PUpperBound PPOSIXTime)
pupperBound = phoistAcyclic $ plam $ \a
  -> let
        ubValue = pcon $ PFinite (pdcons @"_0" # pdata a # pdnil)
        ub      = PUpperBound (pdcons @"_0" # pdata ubValue #$
                               pdcons @"_1" # pdata (pconstant True) # pdnil
                              )
     in
     pcon ub

-- | 'pFrom' is the plutarch level function of 'from'.
pFrom ::
  Term s (PPOSIXTime
    :-->  PInterval PPOSIXTime
         )
pFrom = phoistAcyclic $ plam $ \a
  -> let
      lb      = plowerbound # a
      ubValue = pcon $ PPosInf pdnil
      ub      = pcon $ PUpperBound (pdcons @"_0" # pdata ubValue #$
                                    pdcons @"_1" # pdata (pconstant True) # pdnil
                                   )
     in
      pcon $ PInterval (pdcons @"from" # pdata lb #$
                        pdcons @"to" # pdata ub # pdnil
                       )

-- | 'pTo' is the plutarch level function of 'to'.
pTo ::
  Term s (PPOSIXTime
    :-->  PInterval PPOSIXTime
         )
pTo = phoistAcyclic $ plam $ \a
  -> let
      lbValue = pcon $ PNegInf pdnil
      lb      = pcon $ PLowerBound (pdcons @"_0" # pdata lbValue #$
                                    pdcons @"_1" # pdata (pconstant True) # pdnil
                                   )
      ub      = pupperBound # a
     in
      pcon $ PInterval (pdcons @"from" # pdata lb #$
                        pdcons @"to" # pdata ub # pdnil
                       )

-- | 'pinterval' is plutarch level function of 'interval'.
pinterval ::
  Term s (PPOSIXTime
    :-->  PPOSIXTime
    :-->  PInterval PPOSIXTime
         )
pinterval = phoistAcyclic $ plam $ \lowerB upperB
  -> let
      lb = plowerbound # lowerB
      ub = pupperBound # upperB
     in
      pcon $ PInterval (pdcons @"from" # pdata lb #$
                        pdcons @"to" # pdata ub # pdnil
                       )
