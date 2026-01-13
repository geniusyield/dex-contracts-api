module GeniusYield.Server.Constants (
  module GeniusYield.Api.DEX.Constants,
  gitHash,
) where

import GeniusYield.Api.DEX.Constants
import GitHash
import RIO

-- | The git hash of the current commit.
gitHash :: String
gitHash = either (const "UNKNOWN_REVISION") giHash $$tGitInfoCwdTry
