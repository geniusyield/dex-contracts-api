module GeniusYield.OnChain.Common.Scripts.DEX.Data (
  orderValidator,
  nftPolicy,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.FileEmbed
import PlutusLedgerApi.V1 (Address)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply (
  ScriptRole (..),
  TypedScript,
 )
import Ply.Core.Internal.Reify (ReifyRole, ReifyTypenames)
import Ply.Core.TypedReader (mkTypedScript)

readScript ∷ ∀ r l. (ReifyRole r, ReifyTypenames l) ⇒ ByteString → TypedScript r l
readScript bs =
  let envelope = either (error "GeniusYield.OnChain.Common.Scripts.DEX.Data.readScript: Failed to read envelope") id $ Aeson.eitherDecodeStrict' bs
   in either (error "GeniusYield.OnChain.Common.Scripts.DEX.Data.readScript: Failed to create typed script") id $ mkTypedScript @r @l envelope

orderValidator ∷ (TypedScript 'ValidatorRole '[Address, AssetClass])
orderValidator =
  let fileBS = $(makeRelativeToProject "./data/compiled-scripts/DEX.PartialOrder" >>= embedFile)
   in readScript fileBS

nftPolicy ∷ (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
nftPolicy =
  let fileBS = $(makeRelativeToProject "./data/compiled-scripts/DEX.PartialOrderNFT" >>= embedFile)
   in readScript fileBS
