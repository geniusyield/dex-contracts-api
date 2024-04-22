{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}

module GeniusYield.OnChain.Utils
  ( paidValue
  , paidValue'
  , integerToBuiltinByteString, builtinByteStringToHex
  , ceiling
  , mintedTokens
  , notSignedBy
  , desiredTracingMode
  ) where

import           Plutarch           (TracingMode (DoTracing))
import           PlutusLedgerApi.V1
import qualified PlutusTx.AssocMap  as Map
import           PlutusTx.Prelude
import           PlutusTx.Ratio

{-# INLINABLE paidValue #-}
paidValue :: ScriptContext -> Address -> Value
paidValue ctx' = case scriptContextPurpose ctx' of
  Spending x -> paidValue' x $ scriptContextTxInfo ctx'
  _          -> error ()

{-# INLINABLE paidValue' #-}
paidValue' :: TxOutRef -> TxInfo -> Address -> Value
paidValue' ownUTxO' info' addr = go $ txInfoOutputs info'
  where
    go :: [TxOut] -> Value
    go xs =
      let
        o = head xs
      in
        if p o
            then txOutValue o
            else go $ tail xs

    expectedHash :: Maybe DatumHash
    expectedHash = go' $ txInfoData info'
      where
        go' :: [(DatumHash, Datum)] -> Maybe DatumHash
        go' xs =
          let
            (dh, d) = head xs
          in
            if d == expectedDatum
                then Just dh
                else go' $ tail xs

    expectedDatum :: Datum
    expectedDatum = Datum $ toBuiltinData ownUTxO'

    p :: TxOut -> Bool
    p o = (txOutAddress o == addr) && (txOutDatumHash o == expectedHash)

{-# INLINABLE integerToBuiltinByteString #-}
integerToBuiltinByteString :: Integer -> BuiltinByteString
integerToBuiltinByteString n
    | n < 0     = traceError "only non-negative Integers can be converted"
    | n == 0    = 48 `consByteString` emptyByteString
    | otherwise = go n emptyByteString
  where
    go :: Integer -> BuiltinByteString -> BuiltinByteString
    go m acc
        | m == 0    = acc
        | otherwise =
              let
                m' = m `divide` 10
                r  = m `modulo` 10
              in
                go m' $ consByteString (r + 48) acc

{-# INLINABLE builtinByteStringToHex #-}
builtinByteStringToHex :: BuiltinByteString -> BuiltinByteString
builtinByteStringToHex s = go (lengthOfByteString s - 1) emptyByteString
  where
    go :: Integer -> BuiltinByteString -> BuiltinByteString
    go i acc
        | i < 0     = acc
        | otherwise = go (i - 1) $ appendByteString (byteToBuiltinByteString $ indexByteString s i) acc

{-# INLINABLE byteToBuiltinByteString #-}
byteToBuiltinByteString :: Integer -> BuiltinByteString
byteToBuiltinByteString n = consByteString (digitToByte h) $ consByteString (digitToByte l) emptyByteString
  where
    h = divide n 16
    l = modulo n 16

    digitToByte :: Integer -> Integer
    digitToByte x
        | x <= 9    = x + 48
        | otherwise = x + 87

{-# INLINABLE ceiling #-}
ceiling :: Rational -> Integer
ceiling x
    | x < zero  = truncate x
    | x == y    = truncate x
    | otherwise = 1 + truncate x
  where
    y = fromInteger $ truncate x

{-# INLINABLE mintedTokens #-}
mintedTokens :: CurrencySymbol -> TokenName -> TxInfo -> Integer
mintedTokens cs tn info =
  let
    Just m = Map.lookup cs $ getValue $ txInfoMint info
  in
    fromMaybe 0 $ Map.lookup tn m

notSignedBy :: TxInfo -> PubKeyHash -> Bool
notSignedBy info' pkh' = go $ txInfoSignatories info'
  where
    go :: [PubKeyHash] -> Bool
    go xs = null xs || (head xs /= pkh' && go (tail xs))

desiredTracingMode :: TracingMode
desiredTracingMode = DoTracing
