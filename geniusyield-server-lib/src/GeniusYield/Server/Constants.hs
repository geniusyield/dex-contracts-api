module GeniusYield.Server.Constants (
  poRefsMainnet,
  poRefsPreprod,
  poConfigAddrMainnet,
  poConfigAddrPreprod,
  gitHash,
) where

import GeniusYield.Api.Dex.PartialOrder (PORefs (..))
import GeniusYield.Imports ((&))
import GeniusYield.Types (GYAddress, unsafeAddressFromText)
import GitHash

poRefsMainnet ∷ PORefs
poRefsMainnet =
  PORefs
    { porValRef = "062f97b0e64130bc18b4a227299a62d6d59a4ea852a4c90db3de2204a2cd19ea#2",
      porRefNft = "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.4aff78908ef2dce98bfe435fb3fd2529747b1c4564dff5adebedf4e46d0fc63d",
      porMintRef = "062f97b0e64130bc18b4a227299a62d6d59a4ea852a4c90db3de2204a2cd19ea#1"
    }

poRefsPreprod ∷ PORefs
poRefsPreprod =
  PORefs
    { porValRef = "be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050#2",
      porRefNft = "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.8309f9861928a55d37e84f6594b878941edce5e351f7904c2c63b559bde45c5c",
      porMintRef = "be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050#1"
    }

poConfigAddrMainnet ∷ GYAddress
poConfigAddrMainnet = unsafeAddressFromText "addr1w9zr09hgj7z6vz3d7wnxw0u4x30arsp5k8avlcm84utptls8uqd0z"

poConfigAddrPreprod ∷ GYAddress
poConfigAddrPreprod = unsafeAddressFromText "addr_test1wrgvy8fermjrruaf7fnndtmpuw4xx4cnvfqjp5zqu8kscfcvh32qk"

-- | The git hash of the current commit.
gitHash ∷ String
gitHash = $$tGitInfoCwd & giHash