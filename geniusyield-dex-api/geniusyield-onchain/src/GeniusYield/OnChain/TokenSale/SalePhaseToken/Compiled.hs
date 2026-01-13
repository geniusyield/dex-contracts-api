module GeniusYield.OnChain.TokenSale.SalePhaseToken.Compiled (
  originalSalePhaseTokenPolicy,
  optimizedSalePhaseTokenPolicy,
) where

import Data.Default (Default (def))
import Data.Text (Text)
import GeniusYield.OnChain.TokenSale.SalePhaseToken
import GeniusYield.Plutonomy ()
import Plutarch.Api.V1
import Plutarch.Api.V2 qualified as PV2
import Plutarch.Prelude
import Plutonomy qualified
import PlutusLedgerApi.V1
import Ply hiding ((#))
import Ply.Plutarch (toTypedScript)

-- originalSalePhaseTokenPolicy :: TokenName -> POSIXTime -> POSIXTime -> Address -> Plutonomy.MintingPolicy
-- originalSalePhaseTokenPolicy tn s t a =
--     let
--       mintingPolicy = mkSalePhaseTokenPolicy # pconstant tn # pconstant s # pconstant t # pconstant a
--     in
--     plutonomyMintingPolicyFromScript (Plutarch.compile mintingPolicy)

originalSalePhaseTokenPolicy
  :: Either
       Text
       ( TypedScript
           'MintingPolicyRole
           [ TokenName,
             POSIXTime,
             POSIXTime,
             Address
           ]
       )
originalSalePhaseTokenPolicy = toTypedScript def mkSalePhaseTokenPolicy'

optimizedSalePhaseTokenPolicy
  :: Either
       Text
       ( TypedScript
           'MintingPolicyRole
           '[TokenName, POSIXTime, POSIXTime, Address]
       )
optimizedSalePhaseTokenPolicy = Plutonomy.optimizeUPLC <$> originalSalePhaseTokenPolicy

mkSalePhaseTokenPolicy'
  :: ClosedTerm
       ( PTokenName
           :--> PPOSIXTime
           :--> PPOSIXTime
           :--> PAddress
           :--> PV2.PMintingPolicy
       )
mkSalePhaseTokenPolicy' = plam $ \tn startT endT addr _ ctx ->
  popaque $
    mkSalePhaseTokenPolicy
      # tn
      # startT
      # endT
      # addr
      # pconstantData ()
      # ctx
