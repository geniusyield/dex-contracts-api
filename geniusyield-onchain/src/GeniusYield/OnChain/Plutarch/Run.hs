{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GeniusYield.OnChain.Plutarch.Run
    ( applyArguments
    , evalT
    , evalSerialize
    , evalWithArgsT
    , evalWithArgsT'
    ) where

import           Control.Lens.Combinators (over)
import           Data.Bifunctor           (first)
import           Data.ByteString.Short    (ShortByteString)
import           Data.Default             (def)
import           Data.Text                (Text, pack)
import           Plutarch                 (ClosedTerm, compile)
import           Plutarch.Evaluate        (evalScript)
import           Plutarch.Script          (Script (Script, unScript), serialiseScript)
import           PlutusCore.MkPlc         (mkConstant, mkIterApp)
import           PlutusLedgerApi.V1       (Data, ExBudget)
import           UntypedPlutusCore        (DeBruijn, DefaultFun, DefaultUni, Program, progTerm)

applyArguments :: Script -> [Data] -> Script
applyArguments (Script p) args =
    let termArgs = mkConstant () <$> args
        applied t = mkIterApp () t termArgs
    in Script $ over progTerm applied p

evalSerialize :: ClosedTerm a -> Either Text ShortByteString
evalSerialize x = serialiseScript . (\(a, _, _) -> a) <$> evalT x

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile def x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
evalWithArgsT' x args =
  (\(res, budg, trcs) -> (unScript res, budg, trcs))
    <$> evalWithArgsT x args
