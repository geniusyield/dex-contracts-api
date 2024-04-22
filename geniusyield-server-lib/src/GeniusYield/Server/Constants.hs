module GeniusYield.Server.Constants (
  module GeniusYield.Api.Dex.Constants,
  gitHash,
) where

import GeniusYield.Api.Dex.Constants
import GitHash
import RIO

-- | The git hash of the current commit.
gitHash ∷ String
gitHash = $$tGitInfoCwd & giHash
