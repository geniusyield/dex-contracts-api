module GeniusYield.Server.Assets (
  AssetsAPI,
  handleAssetsApi,
) where

import Fmt
import GeniusYield.OrderBot.Domain.Assets
import GeniusYield.Server.Ctx
import GeniusYield.Server.Utils
import GeniusYield.Types
import Servant

type AssetsAPI = Summary "Get assets information" :> Description "Get information for a specific asset." :> Capture "asset" GYAssetClass :> Get '[JSON] AssetDetails

handleAssetsApi ∷ Ctx → GYAssetClass → IO AssetDetails
handleAssetsApi ctx@Ctx {..} ac = do
  logInfo ctx $ "Fetching details of asset: " +|| ac ||+ ""
  getAssetDetails ctxMaestroProvider ac