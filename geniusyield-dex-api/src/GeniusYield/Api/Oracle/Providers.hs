{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeniusYield.Api.Oracle.Providers (
  OracleProviderInitError (..),
  OracleProviderBundle (..),
  buildOracleProviderBundle,
  resolveAssetMappings,
  applyAssetMappings,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Ratio (approxRational)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime, secondsToNominalDiffTime)
import GeniusYield.Api.Oracle
import GeniusYield.Config.DEX
import GeniusYield.Types
import Maestro.Client.Env qualified as MaestroEnv
import Maestro.Client.V1.Assets qualified as MaestroAssets
import Maestro.Client.V1.DefiMarkets qualified as MaestroMarkets
import Maestro.Types.Common qualified as MaestroCommon
import Maestro.Types.V1 qualified as MaestroTypes
import Network.HTTP.Client (Request)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Servant.API (FromHttpApiData (..), Get, JSON, Post, QueryParam, ReqBody, type (:>))
import Servant.Client qualified as Servant
import Web.HttpApiData (ToHttpApiData (..))

--------------------------------------------------------------------------------
-- Aggregated bundle -----------------------------------------------------------
--------------------------------------------------------------------------------

newtype OracleProviderInitError = OracleProviderInitError {getOracleProviderInitError :: Text}
  deriving stock (Eq, Show)

type MetadataResolver = GYAssetClass -> IO (Maybe Int)

data ProviderBuildResult = ProviderBuildResult
  { pbrProvider :: !PriceProvider,
    pbrDiag :: !Text,
    pbrResolver :: !(Maybe MetadataResolver)
  }

data OracleProviderBundle = OracleProviderBundle
  { opbCfg :: !OracleCfg,
    opbProviders :: !(NonEmpty PriceProvider),
    opbWeights :: !(NonEmpty Int),
    opbDiagnostics :: ![Text]
  }

resolveAssetMappings
  :: [GYOracleAssetMappingConfig]
  -> Either OracleProviderInitError (Map.Map GYAssetClass GYAssetClass, [Text])
resolveAssetMappings mappingCfgs = do
  pairs <- traverse parseMapping mappingCfgs
  let mappingMap = Map.fromList pairs
  if Map.size mappingMap /= length pairs
    then Left $ OracleProviderInitError "duplicate preprod_asset entries in asset_mappings"
    else Right (mappingMap, map renderMapping mappingCfgs)
 where
  parseMapping GYOracleAssetMappingConfig {..} =
    (,) <$> wrap "preprod_asset" goamcPreprodAsset <*> wrap "mainnet_asset" goamcMainnetAsset
  wrap label raw = first (OracleProviderInitError . mkError label raw) (parseAsset raw)

  renderMapping GYOracleAssetMappingConfig {..} =
    Text.unwords ["asset-map", goamcPreprodAsset, "->", goamcMainnetAsset]

  mkError label raw err = Text.unwords ["asset_mappings", label <> "=" <> raw, "-", err]

  parseAsset rawInput
    | lowered == "lovelace" = Right GYLovelace
    | lowered == "ada" = Right GYLovelace
    | otherwise =
        case Text.breakOn "." trimmed of
          (policyTxt, rest)
            | Text.null rest -> Left "expected format policyId.assetNameHex"
            | otherwise ->
                let tokenHex = Text.drop 1 rest
                 in case mintingPolicyIdFromText policyTxt of
                      Left err -> Left (Text.pack err)
                      Right policyId ->
                        case tokenNameFromHex tokenHex of
                          Left err -> Left err
                          Right tokenName -> Right (GYToken policyId tokenName)
   where
    trimmed = Text.strip rawInput
    lowered = Text.toLower trimmed

applyAssetMappings
  :: GYPaymentSigningKey
  -> Map.Map GYAssetClass GYAssetClass
  -> PriceProvider
  -> PriceProvider
applyAssetMappings signingKey mapping provider =
  let
    remap asset = Map.findWithDefault asset asset mapping
    originalGet = ppGet provider
    retag base quote cert =
      let
        Price price = ocPrice cert
        timestamp = ocTimestamp cert
       in
        mkOracleCertificate signingKey base quote price timestamp
   in
    provider
      { ppGet = \base quote -> do
          let
            mappedBase = remap base
            mappedQuote = remap quote
          result <- originalGet mappedBase mappedQuote
          pure $ case result of
            Left err -> Left err
            Right cert -> Right (retag base quote cert)
      }

buildOracleProviderBundle
  :: GYNetworkId
  -> GYPaymentSigningKey
  -> GYOracleConfig
  -> IO (Either OracleProviderInitError OracleProviderBundle)
buildOracleProviderBundle _ signingKey cfg = runExceptT $ do
  let
    cfgBase = defaultOracleCfg
    cfgThreshold1 = fromMaybe (ocThreshold1 cfgBase) (gocThreshold1 cfg)
    cfgThreshold2 = fromMaybe (ocThreshold2 cfgBase) (gocThreshold2 cfg)
    cfgCache = maybe (ocCacheDuration cfgBase) (secondsToNominalDiffTime . fromIntegral . max 0) (gocCacheSeconds cfg)
    oracleCfg = cfgBase {ocThreshold1 = cfgThreshold1, ocThreshold2 = cfgThreshold2, ocCacheDuration = cfgCache}
    mappingCfgs = fromMaybe [] (gocAssetMappings cfg)
  (mappingMap, mappingDiags) <- case resolveAssetMappings mappingCfgs of
    Left err -> throwError err
    Right result -> pure result
  let
    providerCfgs = NE.toList (gocProviders cfg)
    indexedCfgs = zip [0 :: Int ..] providerCfgs
    maestroCfgs = [(idx, maestroCfg) | (idx, GYOracleProviderMaestro maestroCfg) <- indexedCfgs]

  maestroPairs <- forM maestroCfgs $ \(idx, maestroCfg) -> do
    res <- buildMaestroProvider signingKey maestroCfg
    pure (idx, res)
  let maestroResults = Map.fromList maestroPairs

  let metadataResolvers = mapMaybe pbrResolver (Map.elems maestroResults)

  providerResults <- forM indexedCfgs $ \case
    (idx, GYOracleProviderMaestro _) -> case Map.lookup idx maestroResults of
      Just res -> pure res
      Nothing -> throwError $ OracleProviderInitError "internal error: missing Maestro provider"
    (_, GYOracleProviderTaptools tapCfg) -> buildTaptoolsProvider metadataResolvers signingKey tapCfg
    (_, GYOracleProviderCharli3 c3Cfg) -> buildCharli3Provider signingKey c3Cfg

  let mappingWrap = applyAssetMappings signingKey mappingMap
  case providerResults of
    [] -> throwError $ OracleProviderInitError "oracle providers list cannot be empty"
    (firstRes : restRes) -> do
      let
        providerNE = fmap mappingWrap (pbrProvider firstRes :| map pbrProvider restRes)
        diags = mappingDiags ++ (pbrDiag firstRes : map pbrDiag restRes)
        count = 1 + length restRes
      weights <- case gocWeights cfg of
        Nothing -> pure (NE.fromList (replicate count 1))
        Just ws ->
          if length ws == count
            then pure ws
            else throwError $ OracleProviderInitError "oracle provider weights length mismatch"
      pure
        OracleProviderBundle
          { opbCfg = oracleCfg,
            opbProviders = providerNE,
            opbWeights = weights,
            opbDiagnostics = diags
          }

--------------------------------------------------------------------------------
-- Provider builders -----------------------------------------------------------
--------------------------------------------------------------------------------

buildMaestroProvider
  :: GYPaymentSigningKey
  -> MaestroOracleProviderConfig
  -> ExceptT OracleProviderInitError IO ProviderBuildResult
buildMaestroProvider signingKey MaestroOracleProviderConfig {..} = do
  dex <- parseParam "dex" mopcDex
  resolution <- traverse (parseParam "resolution") mopcResolution
  env <- liftIO $ MaestroEnv.mkMaestroEnv @'MaestroEnv.V1 mopcApiKey MaestroEnv.Mainnet MaestroEnv.defaultBackoff
  cache <- liftIO $ newMVar Map.empty
  let
    maestroGet = makeMaestroGet dex resolution
    resolver :: MetadataResolver
    resolver asset = case asset of
      GYLovelace -> pure (Just 6)
      GYToken pid tn -> do
        let token = MaestroTypes.NonAdaNativeToken (MaestroCommon.PolicyId $ mintingPolicyIdToText pid) (MaestroCommon.TokenName $ tokenNameToHex tn)
        outcome <- try @SomeException $ MaestroAssets.assetInfo env token
        pure $ case outcome of
          Left _ -> Nothing
          Right info -> do
            let registry = MaestroTypes.assetInfoTokenRegistryMetadata (MaestroTypes.timestampedAssetInfoData info)
            decimals <- MaestroTypes.tokenRegistryMetadataDecimals =<< registry
            Just (fromIntegral decimals)

    provider =
      PriceProvider
        { ppName = "maestro",
          ppGet = maestroGet env cache
        }
    diag =
      Text.unwords
        [ "maestro",
          "dex=" <> Text.pack (show dex),
          "resolution=" <> maybe "default" (Text.pack . show) resolution,
          maybe "override=none" (\o -> "override=" <> mpocPair o <> if mpocCommodityIsFirst o then "(asset=coinA)" else "(asset=coinB)") maestroOverride,
          "network=mainnet"
        ]
  pure
    ProviderBuildResult
      { pbrProvider = provider,
        pbrDiag = Text.strip diag,
        pbrResolver = Just resolver
      }
 where
  maestroOverride = (mopcPairOverrides >>= NE.nonEmpty) <&> NE.head
  parseParam :: FromHttpApiData a => Text -> Text -> ExceptT OracleProviderInitError IO a
  parseParam label value = case parseQueryParam value of
    Left err -> throwError $ OracleProviderInitError $ "Maestro " <> label <> " parse error: " <> err
    Right v -> pure v

  makeMaestroGet
    :: MaestroTypes.Dex
    -> Maybe MaestroTypes.Resolution
    -> MaestroEnv.MaestroEnv 'MaestroEnv.V1
    -> MVar (Map.Map GYAssetClass MaestroPairSelection)
    -> GYAssetClass
    -> GYAssetClass
    -> IO (Either String OracleCertificate)
  makeMaestroGet dexLocal resLocal env cache base quote
    | base == quote = Right . mkOracleCertificate signingKey base quote 1 <$> getCurrentTime
    | otherwise =
        case classifyPair base quote of
          Nothing -> pure $ Left "maestro supports only ADA pairs"
          Just (asset, orientation) -> runExceptT $ do
            selection <- resolvePair env cache asset
            price <- fetchPrice env selection
            now <- liftIO getCurrentTime
            let ratio = case orientation of
                  BaseOverAda -> price
                  AdaOverBase -> if price == 0 then 0 else recip price
            if ratio <= 0
              then throwError "maestro returned non-positive price"
              else pure (mkOracleCertificate signingKey base quote ratio now)
   where
    classifyPair :: GYAssetClass -> GYAssetClass -> Maybe (GYAssetClass, Orientation)
    classifyPair b q = case (b, q) of
      (asset, GYLovelace) | asset /= GYLovelace -> Just (asset, BaseOverAda)
      (GYLovelace, asset) | asset /= GYLovelace -> Just (asset, AdaOverBase)
      _ -> Nothing

    resolvePair
      :: MaestroEnv.MaestroEnv 'MaestroEnv.V1
      -> MVar (Map.Map GYAssetClass MaestroPairSelection)
      -> GYAssetClass
      -> ExceptT String IO MaestroPairSelection
    resolvePair env' cache' asset = ExceptT $ modifyMVar cache' $ \mp -> case Map.lookup asset mp of
      Just sel -> pure (mp, Right sel)
      Nothing -> do
        result <- discover env' asset
        case result of
          Left err -> pure (mp, Left err)
          Right sel -> pure (Map.insert asset sel mp, Right sel)

    discover
      :: MaestroEnv.MaestroEnv 'MaestroEnv.V1
      -> GYAssetClass
      -> IO (Either String MaestroPairSelection)
    discover env' asset = do
      response <- MaestroMarkets.pairsFromDex env' dexLocal
      let entries = MaestroTypes.dexPairResponsePairs response
      pure $ maybe (Left $ "no ADA pair for asset " <> show asset) Right (findMatch entries)
     where
      findMatch :: [MaestroTypes.DexPairInfo] -> Maybe MaestroPairSelection
      findMatch = foldr (\dpi acc -> acc <|> match dpi) Nothing

      match :: MaestroTypes.DexPairInfo -> Maybe MaestroPairSelection
      match MaestroTypes.DexPairInfo {..} = do
        coinA <- either (const Nothing) Just $ toAsset dexPairInfoCoinAAssetName dexPairInfoCoinAPolicy
        coinB <- either (const Nothing) Just $ toAsset dexPairInfoCoinBAssetName dexPairInfoCoinBPolicy
        if coinA == asset && coinB == GYLovelace
          then
            Just MaestroPairSelection {mpsPair = MaestroTypes.TaggedText dexPairInfoPair, mpsAssetIsCoinA = True}
          else
            if coinB == asset && coinA == GYLovelace
              then
                Just MaestroPairSelection {mpsPair = MaestroTypes.TaggedText dexPairInfoPair, mpsAssetIsCoinA = False}
              else Nothing

      toAsset :: MaestroCommon.TokenName -> MaestroCommon.PolicyId -> Either String GYAssetClass
      toAsset (MaestroCommon.TokenName name) (MaestroCommon.PolicyId policy)
        | Text.null name && Text.null policy = Right GYLovelace
        | otherwise = parseAssetClassWithSep '#' (policy <> "#" <> name)

    fetchPrice
      :: MaestroEnv.MaestroEnv 'MaestroEnv.V1
      -> MaestroPairSelection
      -> ExceptT String IO Rational
    fetchPrice env' MaestroPairSelection {..} = do
      result <- liftIO $ try @SomeException $ MaestroMarkets.pricesFromDex env' dexLocal mpsPair resLocal Nothing Nothing (Just 1) (Just MaestroTypes.Descending)
      case result of
        Left err -> throwError $ "maestro pricesFromDex failed: " <> displayException err
        Right candles -> case candles of
          [] -> throwError "maestro returned empty candle set"
          (latest : _) -> do
            let
              priceRaw
                | mpsAssetIsCoinA = MaestroTypes.ohlcCandleInfoCoinBClose latest
                | otherwise = MaestroTypes.ohlcCandleInfoCoinAClose latest
              ratio = approxRational priceRaw 1.0e-12
            pure ratio

data MaestroPairSelection = MaestroPairSelection
  { mpsPair :: !(MaestroTypes.TaggedText MaestroTypes.PairOfDexTokens),
    mpsAssetIsCoinA :: !Bool
  }

--------------------------------------------------------------------------------
-- Taptools provider -----------------------------------------------------------
--------------------------------------------------------------------------------

buildTaptoolsProvider
  :: [MetadataResolver]
  -> GYPaymentSigningKey
  -> TaptoolsOracleProviderConfig
  -> ExceptT OracleProviderInitError IO ProviderBuildResult
buildTaptoolsProvider resolvers signingKey TaptoolsOracleProviderConfig {..} = do
  env <- ExceptT $ Right <$> taptoolsEnv topcApiKey
  let
    provider =
      PriceProvider
        { ppName = "taptools",
          ppGet = taptoolsGet env
        }
    precisionLabel
      | overridePresent = "precision=override"
      | otherwise = if null resolvers then "precision=auto-missing" else "precision=auto"
    overrideLabel =
      let
        overridesList = fromMaybe [] topcPairOverrides
        rendered = mapMaybe renderOverride overridesList
       in
        if null rendered then "override=none" else "override=" <> Text.intercalate "," rendered
    diag =
      Text.unwords
        [ "taptools",
          precisionLabel,
          overrideLabel
        ]
  pure
    ProviderBuildResult
      { pbrProvider = provider,
        pbrDiag = Text.strip diag,
        pbrResolver = Nothing
      }
 where
  overridePresent = maybe False (not . null) topcPairOverrides

  renderOverride TaptoolsPairOverrideConfig {..} =
    case parseAssetClassWithSep '.' tppcAsset of
      Left _ -> Nothing
      Right _ -> Just (tppcAsset <> "@" <> Text.pack (show tppcPrecision))

  taptoolsGet
    :: Servant.ClientEnv
    -> GYAssetClass
    -> GYAssetClass
    -> IO (Either String OracleCertificate)
  taptoolsGet env base quote
    | base == quote = Right . mkOracleCertificate signingKey base quote 1 <$> getCurrentTime
    | otherwise =
        case classifyPair base quote of
          Nothing -> pure $ Left "taptools supports only ADA pairs"
          Just (asset, orientation) -> runExceptT $ do
            price <- taptoolsFetch env asset
            now <- liftIO getCurrentTime
            let ratio = case orientation of
                  BaseOverAda -> price
                  AdaOverBase -> if price == 0 then 0 else recip price
            if ratio <= 0
              then throwError "taptools returned non-positive price"
              else pure (mkOracleCertificate signingKey base quote ratio now)

  classifyPair :: GYAssetClass -> GYAssetClass -> Maybe (GYAssetClass, Orientation)
  classifyPair base quote = case (base, quote) of
    (asset, GYLovelace) | asset /= GYLovelace -> Just (asset, BaseOverAda)
    (GYLovelace, asset) | asset /= GYLovelace -> Just (asset, AdaOverBase)
    _ -> Nothing

  taptoolsFetch :: Servant.ClientEnv -> GYAssetClass -> ExceptT String IO Rational
  taptoolsFetch env asset
    | asset == GYLovelace = pure 1
    | otherwise = do
        precision <- resolvePrecision asset
        let unit = TtUnit asset
        response <- liftIO $ Servant.runClientM (taptoolsPriceClient [unit]) env
        priceRaw <- case response of
          Left err -> throwError $ "taptools request failed: " <> show err
          Right mp -> case Map.lookup unit mp of
            Nothing -> throwError $ "taptools price missing for unit: " <> Text.unpack (toQueryParam unit)
            Just val -> pure val
        let
          adaPrecision = 6 :: Int
          scale = 10 ** fromIntegral (adaPrecision - precision)
        pure $ approxRational (priceRaw * scale) 1.0e-12

  resolvePrecision :: GYAssetClass -> ExceptT String IO Int
  resolvePrecision asset =
    case overridePrecision asset of
      Just p -> pure p
      Nothing -> do
        if asset == GYLovelace
          then pure 6
          else do
            result <- liftIO $ firstJustM [resolver asset | resolver <- resolvers]
            case result of
              Just p -> pure p
              Nothing -> throwError $ "precision lookup failed for asset " <> show asset

  overridePrecision :: GYAssetClass -> Maybe Int
  overridePrecision asset =
    let
      overridesList = fromMaybe [] topcPairOverrides
      matchOverride TaptoolsPairOverrideConfig {..} =
        case parseAssetClassWithSep '.' tppcAsset of
          Left _ -> Nothing
          Right overrideAsset
            | overrideAsset == asset -> Just (fromIntegral (min 18 tppcPrecision))
            | otherwise -> Nothing
     in
      listToMaybe (mapMaybe matchOverride overridesList)

--------------------------------------------------------------------------------
-- Charli3 provider ------------------------------------------------------------
--------------------------------------------------------------------------------

buildCharli3Provider
  :: GYPaymentSigningKey
  -> Charli3OracleProviderConfig
  -> ExceptT OracleProviderInitError IO ProviderBuildResult
buildCharli3Provider signingKey Charli3OracleProviderConfig {..} = do
  env <- ExceptT $ Right <$> charli3Env c3opcApiKey
  overrides <- traverse parseOverride (fromMaybe [] c3opcPairOverrides)
  let
    overridesMap = Map.fromList overrides
    provider =
      PriceProvider
        { ppName = "charli3",
          ppGet = charli3Get env overridesMap
        }
    diag =
      let rendered = map (renderOverride . fst) overrides
       in Text.unwords
            [ "charli3",
              if null rendered then "override=none" else "override=" <> Text.intercalate "," rendered
            ]
  pure
    ProviderBuildResult
      { pbrProvider = provider,
        pbrDiag = Text.strip diag,
        pbrResolver = Nothing
      }
 where
  parseOverride Charli3PairOverrideConfig {..} = do
    asset <- parseAssetText c3pocAsset
    query <- case (c3pocPolicy, c3pocPool) of
      (Just policy, Nothing) -> pure $ Charli3QueryPolicy (Text.strip policy)
      (Nothing, Just pool) -> pure $ Charli3QueryPool (Text.strip pool)
      (Just _, Just _) -> throwError $ OracleProviderInitError "Charli3 override cannot set both policy and pool"
      (Nothing, Nothing) -> throwError $ OracleProviderInitError "Charli3 override requires policy or pool"
    pure (asset, query)

  parseAssetText raw =
    case parse raw of
      Left err -> throwError $ OracleProviderInitError err
      Right asset -> pure asset
   where
    parse txt
      | lowered == "lovelace" = Right GYLovelace
      | lowered == "ada" = Right GYLovelace
      | otherwise =
          first
            ( \msg ->
                Text.unwords
                  [ "Charli3 override asset parse error",
                    Text.strip txt,
                    "-",
                    Text.pack msg
                  ]
            )
            (parseAssetClassWithSep '.' trimmed)
     where
      trimmed = Text.strip txt
      lowered = Text.toLower trimmed

  renderOverride asset =
    case asset of
      GYLovelace -> "lovelace"
      GYToken pid tn -> mintingPolicyIdToText pid <> "." <> tokenNameToHex tn

  charli3Get
    :: Servant.ClientEnv
    -> Map.Map GYAssetClass Charli3Query
    -> GYAssetClass
    -> GYAssetClass
    -> IO (Either String OracleCertificate)
  charli3Get env overrides base quote
    | base == quote = Right . mkOracleCertificate signingKey base quote 1 <$> getCurrentTime
    | otherwise =
        case classifyPair base quote of
          Nothing -> pure $ Left "charli3 supports only ADA pairs"
          Just (asset, orientation) -> runExceptT $ do
            ratio <- charli3Fetch env overrides asset
            now <- liftIO getCurrentTime
            let price = case orientation of
                  BaseOverAda -> ratio
                  AdaOverBase -> if ratio == 0 then 0 else recip ratio
            if price <= 0
              then throwError "charli3 returned non-positive price"
              else pure (mkOracleCertificate signingKey base quote price now)

  classifyPair base quote = case (base, quote) of
    (asset, GYLovelace) | asset /= GYLovelace -> Just (asset, BaseOverAda)
    (GYLovelace, asset) | asset /= GYLovelace -> Just (asset, AdaOverBase)
    _ -> Nothing

  charli3Fetch env overrides asset
    | asset == GYLovelace = pure 1
    | otherwise = do
        query <- case resolveQuery overrides asset of
          Nothing -> throwError $ "missing policy/pool for asset " <> show asset
          Just q -> pure q
        response <- liftIO $ Servant.runClientM (charli3PriceClient (queryPolicy query) (queryPool query)) env
        case response of
          Left err -> throwError $ "charli3 request failed: " <> show err
          Right resp -> do
            let price = c3crCurrentPrice resp
            if price <= 0
              then throwError "charli3 returned non-positive current_price"
              else pure $ approxRational price 1.0e-12

  resolveQuery overrides asset =
    Map.lookup asset overrides <|> defaultPolicy asset

  defaultPolicy (GYToken pid tn) = Just $ Charli3QueryPolicy (mintingPolicyIdToText pid <> "." <> tokenNameToHex tn)
  defaultPolicy GYLovelace = Nothing

data Charli3Query = Charli3QueryPolicy Text | Charli3QueryPool Text

queryPolicy :: Charli3Query -> Maybe Text
queryPolicy (Charli3QueryPolicy v) = Just v
queryPolicy _ = Nothing

queryPool :: Charli3Query -> Maybe Text
queryPool (Charli3QueryPool v) = Just v
queryPool _ = Nothing

--------------------------------------------------------------------------------
-- Charli3 Servant client ------------------------------------------------------
--------------------------------------------------------------------------------

newtype Charli3CurrentResponse = Charli3CurrentResponse
  { c3crCurrentPrice :: Double
  }

instance Aeson.FromJSON Charli3CurrentResponse where
  parseJSON = Aeson.withObject "Charli3CurrentResponse" $ \o -> do
    Charli3CurrentResponse <$> o .: "current_price"

type Charli3API =
  "tokens"
    :> "current"
    :> QueryParam "policy" Text
    :> QueryParam "pool" Text
    :> Get '[JSON] Charli3CurrentResponse

charli3API :: Proxy Charli3API
charli3API = Proxy

charli3PriceClient :: Maybe Text -> Maybe Text -> Servant.ClientM Charli3CurrentResponse
charli3PriceClient = Servant.client charli3API

charli3Env :: Text -> IO Servant.ClientEnv
charli3Env apiKey = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings {HTTP.managerModifyRequest = addAuth}
  base <- Servant.parseBaseUrl "https://api.charli3.io/api/v1"
  pure $ Servant.mkClientEnv manager base
 where
  header =
    ( "authorization",
      TE.encodeUtf8 $ "Bearer " <> Text.strip apiKey
    )
  addAuth req = pure req {HTTP.requestHeaders = header : filter ((/= "authorization") . fst) (HTTP.requestHeaders req)}

--------------------------------------------------------------------------------
-- Utilities -------------------------------------------------------------------
--------------------------------------------------------------------------------

firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = pure Nothing
firstJustM (x : xs) = do
  r <- x
  case r of
    Just v -> pure (Just v)
    Nothing -> firstJustM xs

data Orientation = BaseOverAda | AdaOverBase

-- Taptools Servant client -----------------------------------------------------
--------------------------------------------------------------------------------

newtype TtUnit = TtUnit GYAssetClass
  deriving stock (Eq, Ord, Show)

instance FromHttpApiData TtUnit where
  parseQueryParam txt = case parseTaptoolsUnit txt of
    Left err -> Left (Text.pack err)
    Right ac -> Right (TtUnit ac)

instance ToHttpApiData TtUnit where
  toQueryParam (TtUnit ac) = case ac of
    GYLovelace -> "lovelace"
    GYToken pid tn -> mintingPolicyIdToText pid <> tokenNameToHex tn

instance Aeson.ToJSON TtUnit where
  toJSON = Aeson.toJSON . toQueryParam

parseTaptoolsUnit :: Text -> Either String GYAssetClass
parseTaptoolsUnit raw
  | cleaned == "lovelace" = Right GYLovelace
  | Text.length cleaned < 56 = Left $ "invalid TapTools unit (too short): " <> Text.unpack raw
  | otherwise =
      let (policyTxt, tokenTxt) = Text.splitAt 56 cleaned
       in case (mintingPolicyIdFromText policyTxt, tokenNameFromHex tokenTxt) of
            (Right policyId, Right tokenName) -> Right (GYToken policyId tokenName)
            _ -> Left $ "invalid TapTools unit: " <> Text.unpack raw
 where
  cleaned = Text.filter (/= '.') raw

instance Aeson.FromJSON TtUnit where
  parseJSON = Aeson.withText "TtUnit" $ \txt -> case parseTaptoolsUnit txt of
    Left err -> fail err
    Right ac -> pure (TtUnit ac)

instance Aeson.FromJSONKey TtUnit where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ \txt -> case parseTaptoolsUnit txt of
    Left err -> fail err
    Right ac -> pure (TtUnit ac)

type TaptoolsAPI =
  "token"
    :> "prices"
    :> ReqBody '[JSON] [TtUnit]
    :> Post '[JSON] (Map.Map TtUnit Double)

taptoolsAPI :: Proxy TaptoolsAPI
taptoolsAPI = Proxy

taptoolsPriceClient :: [TtUnit] -> Servant.ClientM (Map.Map TtUnit Double)
taptoolsPriceClient = Servant.client taptoolsAPI

-- | Run a Taptools request with API key attached as header.
taptoolsEnv :: Text -> IO Servant.ClientEnv
taptoolsEnv apiKey = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings {HTTP.managerModifyRequest = addKey}
  base <- Servant.parseBaseUrl "https://openapi.taptools.io/api/v1"
  pure $ Servant.mkClientEnv manager base
 where
  addKey :: Request -> IO Request
  addKey req = pure req {HTTP.requestHeaders = header : filter ((/= "x-api-key") . fst) (HTTP.requestHeaders req)}
  header = ("x-api-key", TE.encodeUtf8 apiKey)
