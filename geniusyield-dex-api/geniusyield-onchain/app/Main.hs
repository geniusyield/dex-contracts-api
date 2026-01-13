{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Text qualified as Txt
import GeniusYield.OnChain.Core.Common.Scripts
import GeniusYield.OnChain.DEX.NFT.Compiled (optimizedNftPolicy)
import GeniusYield.OnChain.DEX.PartialOrder.Compiled (
  optimizedPartialOrderValidator,
  optimizedPartialOrderValidatorWithTracing,
 )
import GeniusYield.OnChain.DEX.PartialOrderConfig.Compiled (
  optimizedPartialOrderConfigValidator,
  optimizedPartialOrderConfigValidatorWithTracing,
 )
import GeniusYield.OnChain.DEX.PartialOrderNFT.Compiled (
  optimizedPartialOrderNftPolicy,
  optimizedPartialOrderNftPolicyWithTracing,
 )
import GeniusYield.OnChain.DEX.PartialOrderNFTV1_1.Compiled (
  optimizedPartialOrderNftV1_1Policy,
  optimizedPartialOrderNftV1_1PolicyWithTracing,
 )
import GeniusYield.OnChain.DEX.TwoWayOrderConfig.Compiled (
  optimizedTwoWayOrderConfigValidator,
  optimizedTwoWayOrderConfigValidatorWithTracing,
 )
import GeniusYield.OnChain.Staking.Stake.Compiled (optimizedStakeValidator)
import GeniusYield.OnChain.TokenSale.Order.Compiled qualified as TS
import GeniusYield.OnChain.TokenSale.SalePhaseToken.Compiled (optimizedSalePhaseTokenPolicy)
import Ply
import Ply.Core.Internal.Reify (ReifyRole, ReifyTypenames)
import Ply.Core.Serialize
import Ply.Core.TypedReader
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

main :: IO ()
main = do
  createDirectoryIfMissing False scriptStorage
  runExceptT writeScripts >>= \case
    Left e -> throwIO . userError $ Txt.unpack e
    Right a -> pure a

writeScripts :: ExceptT Text IO ()
writeScripts = do
  writeScriptHelper dex'NFTFile optimizedNftPolicy
  writeScriptHelper dex'PartialOrderFile optimizedPartialOrderValidator
  writeScriptHelper dex'PartialOrderFileTracing optimizedPartialOrderValidatorWithTracing
  writeScriptHelper dex'PartialOrderNFTFile optimizedPartialOrderNftPolicy
  writeScriptHelper dex'PartialOrderNFTFileTracing optimizedPartialOrderNftPolicyWithTracing
  writeScriptHelper dex'PartialOrderNFTV1_1File optimizedPartialOrderNftV1_1Policy
  writeScriptHelper dex'PartialOrderNFTV1_1FileTracing optimizedPartialOrderNftV1_1PolicyWithTracing
  writeScriptHelper dex'PartialOrderConfigFile optimizedPartialOrderConfigValidator
  writeScriptHelper dex'PartialOrderConfigFileTracing optimizedPartialOrderConfigValidatorWithTracing
  writeScriptHelper dex'TwoWayOrderConfigFile optimizedTwoWayOrderConfigValidator
  writeScriptHelper dex'TwoWayOrderConfigFileTracing optimizedTwoWayOrderConfigValidatorWithTracing
  writeScriptHelper staking'StakeFile optimizedStakeValidator
  writeScriptHelper tokenSale'OrderFile TS.optimizedOrderValidator
  writeScriptHelper tokenSale'SalePhaseTokenFile optimizedSalePhaseTokenPolicy

scriptStorage :: FilePath
scriptStorage = "compiled"

writeScriptHelper :: (ReifyRole rl, ReifyTypenames params) => FilePath -> Either Text (TypedScript rl params) -> ExceptT Text IO ()
writeScriptHelper name script =
  except script
    >>= liftIO . writeEnvelope (scriptStorage </> name) . typedScriptToEnvelope (Txt.pack name)
