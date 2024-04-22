{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception                                    (throwIO)
import           Control.Monad.IO.Class                               (liftIO)
import           Control.Monad.Trans.Except
import           Data.Text                                            (Text)
import qualified Data.Text                                            as Txt
import           System.Directory                                     (createDirectoryIfMissing)
import           System.FilePath                                      ((</>))

import           Ply
import           Ply.Core.Internal.Reify                              (ReifyRole,
                                                                       ReifyTypenames)
import           Ply.Core.Serialize
import           Ply.Core.TypedReader

import           GeniusYield.OnChain.Common.Scripts

import           GeniusYield.OnChain.DEX.NFT.Compiled                 (optimizedNftPolicy)
import           GeniusYield.OnChain.DEX.PartialOrder.Compiled        (optimizedPartialOrderValidator,
                                                                       optimizedPartialOrderValidatorWithTracing)
import           GeniusYield.OnChain.DEX.PartialOrderConfig.Compiled  (optimizedPartialOrderConfigValidator,
                                                                       optimizedPartialOrderConfigValidatorWithTracing)
import           GeniusYield.OnChain.DEX.PartialOrderNFT.Compiled     (optimizedPartialOrderNftPolicy,
                                                                       optimizedPartialOrderNftPolicyWithTracing)
import           GeniusYield.OnChain.DEX.PartialOrderNFTV1_1.Compiled

main :: IO ()
main = do
    createDirectoryIfMissing False scriptStorage
    runExceptT writeScripts >>= \case
        Left e  -> throwIO . userError $ Txt.unpack e
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

scriptStorage :: FilePath
scriptStorage = "geniusyield-common/data/compiled-scripts"

writeScriptHelper :: (ReifyRole rl, ReifyTypenames params) => FilePath -> Either Text (TypedScript rl params) -> ExceptT Text IO ()
writeScriptHelper name script = except script
    >>= liftIO . writeEnvelope (scriptStorage </> name) . typedScriptToEnvelope (Txt.pack name)
