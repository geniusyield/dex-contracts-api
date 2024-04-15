{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans          #-}

module GeniusYield.OnChain.Plutarch.Crypto ( PPubKey
                                           , PSignature
                                           , PSignedMessage
                                           , pverifySignedMessage
                                           ) where

import           Plutarch.Api.V1
import qualified Plutarch.Api.V2                    as PV2
import           Plutarch.Crypto
import           Plutarch.DataRepr
import           Plutarch.Prelude

import           GeniusYield.OnChain.Plutarch.Api   (pguardC, pmatchC)
import           GeniusYield.OnChain.Plutarch.Utils (pparseDatum)

type PSignature :: PType
type PSignature = PByteString

type PPubKey :: PType
type PPubKey = PByteString

newtype PSignedMessage (a :: PType) (s :: S)
  = PSignedMessage ( Term
                       s
                       ( PDataRecord
                            '[ "signature"   ':= PSignature
                             , "messageHash" ':= PDatumHash
                             ]
                       )
                    )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType (PSignedMessage a) where type DPTStrat _ = PlutusTypeData

pverifySignedMessage :: forall (s :: S) (a :: PType). PTryFrom PData (PAsData a) =>
  Term s ( PPubKey
    :-->   PSignedMessage a
    :-->   PV2.PTxInfo
    :-->   PAsData a
         )
pverifySignedMessage = plam $ \pk signedMsg info
  -> unTermCont $ do
     a@(PDatumHash dh)  <- pmatchC (pfield @"messageHash" # signedMsg)
     let sig            = pfield @"signature" # signedMsg

     pguardC "invalid signature" (pverifyEd25519Signature # pk # dh # sig)

     mtype <- pmatchC $ pparseDatum # pcon a #$ pfield @"datums" # info

     case mtype of
       PNothing -> return $ ptraceError "datum not found"
       PJust x  -> return x
