{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Exception (throwIO)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Txt
import GeniusYield.OnChain.DEX.NFT.Compiled (originalNftPolicy)
import GeniusYield.OnChain.DEX.PartialOrder.Compiled (originalPartialOrderValidator)
import GeniusYield.OnChain.DEX.PartialOrderConfig.Compiled (originalPartialOrderConfigValidator)
import GeniusYield.OnChain.DEX.PartialOrderNFT.Compiled (originalPartialOrderNftPolicy)
import GeniusYield.OnChain.DEX.PartialOrderNFTV1_1.Compiled (originalPartialOrderNftV1_1Policy)
import GeniusYield.OnChain.Staking.Stake.Compiled (originalStakeValidator)
import GeniusYield.OnChain.TokenSale.Order.Compiled qualified as TS (originalOrderValidator)
import GeniusYield.OnChain.TokenSale.SalePhaseToken.Compiled (originalSalePhaseTokenPolicy)
import GeniusYield.Plutonomy (
  plutonomyMintingPolicyFromScript,
  plutonomyValidatorFromScript,
 )
import Plutonomy qualified as P
import Plutonomy.Test (
  TestOptions (TestOptions, toAggSize, toFixturesDir, toName, toOptSize, toTerm, toUnoptSize),
  plutonomyTests,
 )
import PlutusLedgerApi.V1.Address (Address (..))
import PlutusLedgerApi.V1.Credential (Credential (..))
import PlutusLedgerApi.V1.Value (assetClass)
import PlutusTx.Ratio (fromGHC)
import Ply ((#))
import Test.Tasty (defaultMain, testGroup)

getOrthrowText :: Either Text a -> IO a
getOrthrowText = either (throwIO . userError . Txt.unpack) pure

main :: IO ()
main = do
  partialOrderVal <- getOrthrowText $ originalPartialOrderValidator def
  partialOrderNftPolicy <- getOrthrowText $ originalPartialOrderNftPolicy def
  partialOrderNftPolicyV1_1 <- getOrthrowText $ originalPartialOrderNftV1_1Policy def
  partialOrderConfigVal <- getOrthrowText $ originalPartialOrderConfigValidator def
  nftPolicy <- getOrthrowText originalNftPolicy
  stakeVal <- getOrthrowText originalStakeValidator
  salePhaseTokenPolicy <- getOrthrowText originalSalePhaseTokenPolicy
  tsOrderVal <- getOrthrowText TS.originalOrderValidator
  let
    plutonomyRawFromValidator = P.validatorToRaw . plutonomyValidatorFromScript
    plutonomyRawFromMintingPolicy = P.mintingPolicyToRaw . plutonomyMintingPolicyFromScript
  defaultMain $
    testGroup
      "geniusyield"
      [ testGroup
          "DEX"
          [ plutonomyTests
              TestOptions
                { toName = "partialorder",
                  toTerm =
                    plutonomyRawFromValidator $
                      partialOrderVal
                        # anAddress
                        # ac,
                  toUnoptSize = (6_037, 5_629),
                  toOptSize = (5_369, 4_936),
                  toAggSize = (5_369, 4_936),
                  toFixturesDir = "fixtures"
                },
            plutonomyTests
              TestOptions
                { toName = "partialordernftpolicy",
                  toTerm =
                    plutonomyRawFromMintingPolicy $
                      partialOrderNftPolicy
                        # aScriptHash
                        # anAddress
                        # ac,
                  toUnoptSize = (6_040, 5_652),
                  toOptSize = (5_082, 4_546),
                  toAggSize = (5_082, 4_546),
                  toFixturesDir = "fixtures"
                },
            plutonomyTests
              TestOptions
                { toName = "partialordernftpolicyV1_1",
                  toTerm =
                    plutonomyRawFromMintingPolicy $
                      partialOrderNftPolicyV1_1
                        # aScriptHash
                        # anAddress
                        # ac,
                  toUnoptSize = (6_040, 5_652),
                  toOptSize = (5_082, 4_546),
                  toAggSize = (5_082, 4_546),
                  toFixturesDir = "fixtures"
                },
            plutonomyTests
              TestOptions
                { toName = "partialorderconfig",
                  toTerm = plutonomyRawFromValidator $ partialOrderConfigVal # ac,
                  toUnoptSize = (2_978, 2_630),
                  toOptSize = (2_559, 2_250),
                  toAggSize = (2_559, 2_250),
                  toFixturesDir = "fixtures"
                },
            plutonomyTests
              TestOptions
                { toName = "nftpolicy",
                  toTerm = plutonomyRawFromMintingPolicy nftPolicy,
                  toUnoptSize = (507, 440),
                  toOptSize = (445, 388),
                  toAggSize = (445, 388),
                  toFixturesDir = "fixtures"
                }
          ],
        testGroup
          "Staking"
          [ plutonomyTests
              TestOptions
                { toName = "stake",
                  toTerm = plutonomyRawFromValidator stakeVal,
                  toUnoptSize = (704, 633),
                  toOptSize = (611, 555),
                  toAggSize = (611, 555),
                  toFixturesDir = "fixtures"
                }
          ],
        testGroup
          "TokenSale"
          [ plutonomyTests
              TestOptions
                { toName = "sakephasetoken",
                  toTerm = plutonomyRawFromMintingPolicy $ salePhaseTokenPolicy # "TOKEN" # 1_000_000_000 # 2_000_000_000 # anAddress,
                  toUnoptSize = (948, 911),
                  toOptSize = (812, 797),
                  toAggSize = (812, 797),
                  toFixturesDir = "fixtures"
                },
            plutonomyTests
              TestOptions
                { toName = "saleorder",
                  toTerm =
                    plutonomyRawFromValidator $
                      tsOrderVal
                        # 2_000_000_000 -- end sale
                        # 3_000_000_000 -- end distribution
                        # "5e0070a6f24e5f63cfc245f4468e43917719e26ddb7b7a89c0c984c9" -- cs token
                        # "TOKEN2" -- tn token
                        # fromGHC 10 -- price
                        # aPKH -- pkh
                        # 1 -- min alloc
                        # fromGHC 0.05 -- fee
                        # anAddress -- fee address
                        # "TOKEN", -- tn SPT
                  toUnoptSize = (1_987, 1_944),
                  toOptSize = (1_688, 1_693),
                  toAggSize = (1_688, 1_693),
                  toFixturesDir = "fixtures"
                }
          ]
      ]
 where
  aPKH = "e2f9d92651c75a28717bf5622e6164e25133d856e9c02ea21a234dfc"
  aScriptHash = "e2f9d92651c75a28717bf5622e6164e25133d856e9c02ea21a234dfc"
  anAddress = Address (PubKeyCredential "a881d6369fa731377d82d806d8deb2067878129a5e2df96c25e5a08e") Nothing
  cs = "be18c29c7f0ffca5c3e6cd56f97df0f82a31e317e99bfa031b3b0fe3"
  tn = "47454e53"
  ac = assetClass cs tn
