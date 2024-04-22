{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import           Control.Exception                                     (throwIO)
import           Data.Default                                          (def)
import           Data.Text                                             (Text)
import qualified Data.Text                                             as Txt
import qualified Plutonomy                                             as P
import           Plutonomy.Test                                        (TestOptions (TestOptions, toAggSize, toFixturesDir, toName, toOptSize, toTerm, toUnoptSize),
                                                                        plutonomyTests)
import           PlutusLedgerApi.V1.Address                            (Address (..))
import           PlutusLedgerApi.V1.Credential                         (Credential (..))
import           PlutusLedgerApi.V1.Value                              (assetClass)
import           Ply                                                   ((#))
import           Test.Tasty                                            (defaultMain, testGroup)

import           GeniusYield.OnChain.DEX.NFT.Compiled                  (originalNftPolicy)
import           GeniusYield.OnChain.DEX.PartialOrder.Compiled         (originalPartialOrderValidator)
import           GeniusYield.OnChain.DEX.PartialOrderNFT.Compiled      (originalPartialOrderNftPolicy)



import           GeniusYield.OnChain.DEX.PartialOrderConfig.Compiled   (originalPartialOrderConfigValidator)
import           GeniusYield.OnChain.DEX.PartialOrderNFTV1_1.Compiled  (originalPartialOrderNftV1_1Policy)
import           GeniusYield.Plutonomy                                 (plutonomyMintingPolicyFromScript,
                                                                        plutonomyValidatorFromScript)

getOrthrowText :: Either Text a -> IO a
getOrthrowText = either (throwIO . userError . Txt.unpack) pure

main :: IO ()
main = do
    partialOrderVal <- getOrthrowText $ originalPartialOrderValidator def
    partialOrderNftPolicy <- getOrthrowText $ originalPartialOrderNftPolicy def
    partialOrderNftPolicyV1_1 <- getOrthrowText $ originalPartialOrderNftV1_1Policy def
    partialOrderConfigVal <- getOrthrowText $ originalPartialOrderConfigValidator def
    nftPolicy  <- getOrthrowText originalNftPolicy
    let plutonomyRawFromValidator     = P.validatorToRaw . plutonomyValidatorFromScript
        plutonomyRawFromMintingPolicy = P.mintingPolicyToRaw . plutonomyMintingPolicyFromScript
    defaultMain $ testGroup "geniusyield"
        [ testGroup "DEX"
            [ plutonomyTests TestOptions
                { toName        = "partialorder"
                , toTerm        = plutonomyRawFromValidator $ partialOrderVal
                    # anAddress
                    # ac
                , toUnoptSize   = (6_037, 5_629)
                , toOptSize     = (5_369, 4_936)
                , toAggSize     = (5_369, 4_936)
                , toFixturesDir = "fixtures"
                }
            , plutonomyTests TestOptions
                { toName        = "partialordernftpolicy"
                , toTerm        = plutonomyRawFromMintingPolicy $ partialOrderNftPolicy
                    # aScriptHash
                    # anAddress
                    # ac
                , toUnoptSize   = (6_040, 5_652)
                , toOptSize     = (5_082, 4_546)
                , toAggSize     = (5_082, 4_546)
                , toFixturesDir = "fixtures"
                }
            , plutonomyTests TestOptions
                { toName        = "partialordernftpolicyV1_1"
                , toTerm        = plutonomyRawFromMintingPolicy $ partialOrderNftPolicyV1_1
                    # aScriptHash
                    # anAddress
                    # ac
                , toUnoptSize   = (6_040, 5_652)
                , toOptSize     = (5_082, 4_546)
                , toAggSize     = (5_082, 4_546)
                , toFixturesDir = "fixtures"
                }
            , plutonomyTests TestOptions
                { toName        = "partialorderconfig"
                , toTerm        = plutonomyRawFromValidator $ partialOrderConfigVal # ac
                , toUnoptSize   = (2_978, 2_630)
                , toOptSize     = (2_559, 2_250)
                , toAggSize     = (2_559, 2_250)
                , toFixturesDir = "fixtures"
                }
            , plutonomyTests TestOptions
                { toName        = "nftpolicy"
                , toTerm        = plutonomyRawFromMintingPolicy nftPolicy
                , toUnoptSize   = (507,440)
                , toOptSize     = (445,388)
                , toAggSize     = (445,388)
                , toFixturesDir = "fixtures"
                }
            ]
        ]
  where
    aScriptHash = "e2f9d92651c75a28717bf5622e6164e25133d856e9c02ea21a234dfc"
    anAddress = Address (PubKeyCredential "a881d6369fa731377d82d806d8deb2067878129a5e2df96c25e5a08e") Nothing
    cs = "be18c29c7f0ffca5c3e6cd56f97df0f82a31e317e99bfa031b3b0fe3"
    tn = "47454e53"
    ac = assetClass cs tn
