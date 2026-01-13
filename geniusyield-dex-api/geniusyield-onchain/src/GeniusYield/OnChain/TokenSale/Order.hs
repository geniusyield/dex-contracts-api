{-# LANGUAGE DataKinds #-}
{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GeniusYield.OnChain.TokenSale.Order (
  -- * Plutarch types
  (:-->),
  Term,
  PPOSIXTime,
  PCurrencySymbol,
  PRational,
  PPubKeyHash,
  PInteger,

  -- * Types
  TokenSaleParams (..),
  OrderDatum (..),
  OrderAction (..),

  -- * Order validator
  mkOrderValidator,
) where

import GeniusYield.OnChain.Core.Common.TokenSale
import GeniusYield.OnChain.Plutarch.Api
import GeniusYield.OnChain.Plutarch.Utils
import GeniusYield.OnChain.TokenSale.Order.Types
import Plutarch.Api.V1
import Plutarch.Api.V1.Value
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Builtin
import Plutarch.Prelude
import Plutarch.Rational qualified as PRational

{- | 'mkOrderValidator' is the validator that is responsible for filling and cancelling token sale orders.
     It is parameterised over:

     - 'PPOSIXTime': Time when the token sale will end and distribution will begin.
     - 'PPOSIXTime': Time when distribution will end.
     - 'PCurrencySymbol': Currency symbol of the token that's the sale is about.
     - 'PTokenName': Token name of the token that the sale is about.
     - 'PRational': Price of a single token.
     - 'PPubKeyHash': Public key hash of the seller.
     - 'PInteger': Minimum amount of tokens that must given.
     - 'PRational': Fees for the platform provider.
     - 'PAddress': Fee address of the platform provider.
     - 'PTokenName': Token name of the sale phase token.

     The following actions can be performed by the redeemer 'OrderAction':

      - /Fill/ the order

          An order can be filled if the following conditions are statisfied:

           1. The order is filled during the distribution period i.e. between the end of the token sale
              and the end of the distribution.
           2. The platform provider gets the appropriate fees.
           3. The buyer of the token gets tokens and change at the correct price.
           4. The transaction is signed by the seller of the token sale.
           5. The buyer gets at least the minimum allocation of the tokens.

      - /Cancel/ the Order

          Cancelling of the order is valid if the following conditions are
          statisfied:

           1. The cancelling of the order is happening after distribution has ended.
           2. The transaction is signed by the owner of the order.
-}
mkOrderValidator
  :: Term
       s
       ( PPOSIXTime
           :--> PPOSIXTime
           :--> PCurrencySymbol
           :--> PTokenName
           :--> PRational
           :--> PPubKeyHash
           :--> PInteger
           :--> PRational
           :--> PAddress
           :--> PTokenName
           :--> PAsData POrderDatum
           :--> PAsData POrderAction
           :--> PV2.PScriptContext
           :--> PUnit
       )
mkOrderValidator =
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
     orderDatum
     orderAction
     ctx ->
        validator
          # endSaleT
          # endDistribT
          # csToken
          # tnToken
          # price
          # sellerKey
          # minAlloc
          # fee
          # feeAddr
          # tnSPT
          # pfromData orderDatum
          # pfromData orderAction
          # (pownUtxo # ctx)
          # (pfield @"txInfo" # ctx)
 where
  validator
    :: Term
         s
         ( PPOSIXTime -- 'PPOSIXTime' when the token sale will end.
             :--> PPOSIXTime -- 'PPOSIXTime' when the distribution will end.
             :--> PCurrencySymbol -- 'PCurrencySymbol' of the token that's the sale is about.
             :--> PTokenName -- 'PTokenName' of the token that's the sale is about.
             :--> PRational --  price of a single token.
             :--> PPubKeyHash -- 'PPubKeyHash' of the seller.
             :--> PInteger --  minimal token allocation.
             :--> PRational --  fees for the platform provider
             :--> PAddress --  The address where the fees must be sent to.
             :--> PTokenName -- 'PTokenName' of the sales phase token
             :--> POrderDatum -- 'OrderDatum' is the datum of the script
             :--> POrderAction -- 'OrderAction' is the redeemer of the script
             :--> PTxOutRef --  the utxo that script is trying to spend.
             :--> PV2.PTxInfo -- 'TxInfo'
             :--> PUnit
         )
  validator =
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
       orderDatum
       orderAction
       ownUtxo
       info ->
          unTermCont $ do
            orderDatumF <-
              pletFieldsC
                @'[ "ownerAddr",
                    "ownerKey"
                  ]
                orderDatum

            infoF <-
              pletFieldsC
                @'[ "inputs",
                    "outputs"
                  ]
                info

            let
              ownTxOut = pfindTxOutByTxOutRef # ownUtxo # getField @"inputs" infoF
              paidValue =
                ppaidValue
                  # ownUtxo
                  # getField @"ownerAddr" orderDatumF
                  # (pfield @"datums" # info)
                  #$ pfield @"outputs"
                  # info
              paidAmt' = paidAmt # csToken # tnToken # paidValue

              paidFeesLovelace =
                plovelaceValueOf
                  #$ ppaidValue
                  # ownUtxo
                  # feeAddr
                  # (pfield @"datums" # info)
                  #$ pfield @"outputs"
                  # info

            offerAmt' <- pletC (offerAmt #$ pfield @"value" # ownTxOut)
            ownLovelace <- pletC (pfromData $ pfstBuiltin # offerAmt')
            csSPT <- pletC (pfromData $ psndBuiltin # offerAmt')

            isTokensBurnt <- pletC (tokenBurnt # csSPT # tnSPT # getField @"outputs" infoF)

            -- Checks

            -- Check if sale phase token has been burnt regardless of the action.
            pguardC "expected the salePhaseToken to be burnt" isTokensBurnt

            action <- pmatchC orderAction

            case action of
              PCancel _ ->
                return $
                  pif
                    ( validateCancelOrder
                        # getField @"ownerKey" orderDatumF
                        # endDistribT
                        # info
                    )
                    (pconstant ())
                    (ptraceError "not able to Cancel the order.")
              PFill _ ->
                return $
                  pif
                    ( validateFillOrder
                        # price
                        # fee
                        # minAlloc
                        # sellerKey
                        # endSaleT
                        # endDistribT
                        # ownLovelace
                        # paidAmt'
                        # paidFeesLovelace
                        # info
                    )
                    (pconstant ())
                    (ptraceError "not able to Fill the order.")

  validateCancelOrder
    :: Term
         s
         ( PPubKeyHash -- 'PPubKeyHash' of the owner
             :--> PPOSIXTime -- 'PPOSIXTime' when the distribution will end.
             :--> PV2.PTxInfo -- 'TxInfo'
             :--> PBool
         )
  validateCancelOrder = plam $ \ownerKey endDistribT info ->
    unTermCont $ do
      infoF <-
        pletFieldsC
          @'[ "validRange",
              "signatories"
            ]
          info

      let
        canCancel = pcontains # (pFrom # endDistribT) # getField @"validRange" infoF
        signedByOwner = ptxSignedBy # ownerKey # getField @"signatories" infoF

      return (signedByOwner #&& canCancel)

  validateFillOrder
    :: Term
         s
         ( PRational -- price of a token
             :--> PRational -- fee.
             :--> PInteger -- minimal token allocation amount.
             :--> PPubKeyHash -- seller key.
             :--> PPOSIXTime -- end token sale time.
             :--> PPOSIXTime -- end distribution time.
             :--> PInteger -- Lovelace paid for the order
             :--> PBuiltinPair (PAsData PInteger) (PAsData PInteger) -- paid amount (number of Tokens, lovelace)
             :--> PInteger -- fee amount in lovelace
             :--> PV2.PTxInfo -- TxInfo
             :--> PBool
         )
  validateFillOrder =
    plam $
      \price
       fee
       minAlloc
       sellerKey
       endSaleT
       endDistribT
       ownLovelace
       pAmt
       paidFee
       info ->
          unTermCont $ do
            paidLovelace <- pletC (pfromData $ pfstBuiltin # pAmt)
            paidTokens <- pletC (pfromData $ psndBuiltin # pAmt)

            priceLovelace <- pletC (pceiling #$ (PRational.pfromInteger # paidTokens) * price)
            feesLovelace <- pletC (pceiling #$ (PRational.pfromInteger # priceLovelace) * fee)

            let
              validRange = pfromData $ pfield @"validRange" # info
              paidValidAmt = ownLovelace #<= (paidLovelace + priceLovelace + feesLovelace)
              paidValidFees = feesLovelace #<= paidFee

              signedBySeller = ptxSignedBy # sellerKey # (pfield @"signatories" # info)

              canFill = pcontains # (pinterval # endSaleT # endDistribT) # validRange

              paidMinTokens = minAlloc #<= paidTokens

            return
              ( signedBySeller
                  #&& canFill
                  #&& paidValidFees
                  #&& paidValidAmt
                  #&& paidMinTokens
              )

  tokenBurnt
    :: Term
         s
         ( PCurrencySymbol
             :--> PTokenName
             :--> PBuiltinList PV2.PTxOut
             :--> PBool
         )
  tokenBurnt = plam $ \cs tn outputs ->
    precList
      ( \self txout txouts ->
          pif
            (pvalueOf # (pfield @"value" # txout) # cs # tn #== 0)
            (self # txouts)
            (pconstant False)
      )
      (const $ pconstant True)
      # outputs

  offerAmt
    :: Term
         s
         ( PValue 'Sorted any
             :--> PBuiltinPair (PAsData PInteger) (PAsData PCurrencySymbol)
         )
  offerAmt = plam $ \val' ->
    unTermCont $ do
      val <- pletC (pto $ pto val')

      pguardC
        "only ADA and sale phase token expected in Order."
        (plength # val #== 2)

      let
        ownLovelace = plovelaceValueOf # val' - minTokenSaleDeposit
        csSPT =
          precList
            ( \self x xs ->
                plet (pfromData $ pfstBuiltin # x) $ \cs ->
                  pif
                    (pnot #$ cs #== padaSymbol)
                    (pdata cs)
                    (self # xs)
            )
            (const $ ptraceError "there must be one non ada asset class.")
            # val
      return $ ppairDataBuiltin # pdata ownLovelace # csSPT

  paidAmt
    :: Term
         s
         ( PCurrencySymbol
             :--> PTokenName
             :--> PValue 'Sorted any
             :--> PBuiltinPair (PAsData PInteger) (PAsData PInteger)
         )
  paidAmt = plam $ \csToken tnToken val' ->
    unTermCont $ do
      let
        paidLovelace = plovelaceValueOf # val' - minTokenSaleDeposit
        paidTokens = pvalueOf # val' # csToken # tnToken

      return $ ppairDataBuiltin # pdata paidLovelace # pdata paidTokens
