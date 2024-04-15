module GeniusYield.OnChain.DEX.PartialOrderNFTV1_1.Compiled (
  originalPartialOrderNftV1_1Policy,
  optimizedPartialOrderNftV1_1Policy,
  optimizedPartialOrderNftV1_1PolicyWithTracing,
) where

import           Data.Default                                (def)
import           Data.Text                                   (Text)
import           Plutarch                                    (Config (tracingMode))
import           Plutarch.Api.V1                             (PAddress)
import           Plutarch.Api.V1.Scripts                     (PScriptHash)
import qualified Plutarch.Api.V2                             as PV2
import           Plutarch.Prelude                            (ClosedTerm, plam,
                                                              popaque,
                                                              type (:-->), (#))
import qualified Plutarch.Unsafe                             as PUNSAFE
import qualified Plutonomy
import           PlutusLedgerApi.V1                          (Address,
                                                              ScriptHash)
import           PlutusLedgerApi.V1.Value                    (AssetClass)
import           Ply                                         (ScriptRole (MintingPolicyRole),
                                                              TypedScript)
import           Ply.Plutarch                                (toTypedScript)

import           GeniusYield.OnChain.DEX.PartialOrderNFTV1_1 (mkPartialOrderNFTV1_1Policy)
import           GeniusYield.OnChain.Plutarch.Types          (PAssetClass)
import           GeniusYield.OnChain.Utils                   (desiredTracingMode)
import           GeniusYield.Plutonomy                       ()

originalPartialOrderNftV1_1Policy ::
  Config ->
  Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
originalPartialOrderNftV1_1Policy cnf = toTypedScript cnf mkPartialOrderNFTV1_1Policy'

optimizedPartialOrderNftV1_1Policy :: Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
optimizedPartialOrderNftV1_1Policy = Plutonomy.optimizeUPLC <$> originalPartialOrderNftV1_1Policy def

optimizedPartialOrderNftV1_1PolicyWithTracing :: Either Text (TypedScript 'MintingPolicyRole '[ScriptHash, Address, AssetClass])
optimizedPartialOrderNftV1_1PolicyWithTracing = Plutonomy.optimizeUPLC <$> originalPartialOrderNftV1_1Policy def{tracingMode = desiredTracingMode}

mkPartialOrderNFTV1_1Policy' :: ClosedTerm (PScriptHash :--> PAddress :--> PAssetClass :--> PV2.PMintingPolicy)
mkPartialOrderNFTV1_1Policy' = plam $ \sh refInputAddr refInputToken redm ctx ->
  popaque $
    mkPartialOrderNFTV1_1Policy
      # sh
      # refInputAddr
      # refInputToken
      # PUNSAFE.punsafeCoerce redm
      # ctx
