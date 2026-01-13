{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.Scripts.BlueprintTH (
  makeBPTypes,
  uponBPTypes,
) where

import Control.Monad (foldM)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Short qualified as SBS
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Natural (Natural)
import GeniusYield.Imports (Generic, (&))
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types.Blueprint.Contract
import GeniusYield.Types.Blueprint.DefinitionId (DefinitionId, mkDefinitionId, unDefinitionId)
import GeniusYield.Types.Blueprint.Parameter
import GeniusYield.Types.Blueprint.Preamble
import GeniusYield.Types.Blueprint.Schema
import GeniusYield.Types.Blueprint.Validator
import GeniusYield.Types.PlutusVersion (PlutusVersion (..))
import GeniusYield.Types.Script (GYScript, scriptFromSerialisedScript)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PlutusCore qualified as PLC
import PlutusLedgerApi.Common (serialiseUPLC, uncheckedDeserialiseUPLC)
import PlutusLedgerApi.Common qualified as Plutus
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusTx
import UntypedPlutusCore qualified as UPLC

moduleName :: String
moduleName = $(location >>= stringE . loc_module)

createIssue :: String
createIssue = "Please raise an issue if you think it's a valid use case o/w."

sanitize :: Text.Text -> Text.Text
sanitize = Text.map (\c -> if c `elem` supportedChars then c else '_')
 where
  supportedChars = '_' : ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

genTyconName :: DefinitionId -> Name
genTyconName defId = mkName $ Text.unpack $ "BP" <> sanitize (unDefinitionId defId)

stockDerivations :: [DerivClause]
stockDerivations = [DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Show, ConT ''Ord, ConT ''Generic]]

decTypes :: DefinitionId -> Schema -> Q [Dec]
decTypes defId = \case
  SchemaInteger _ _ -> pure [TySynD tyconName [] (ConT ''Integer)]
  SchemaBytes _ _ -> pure [TySynD tyconName [] (ConT ''Plutus.BuiltinByteString)]
  SchemaList _ MkListSchema {..} -> case lsItems of
    ListItemSchemaSchema s -> case s of
      SchemaDefinitionRef itemDefId ->
        pure [TySynD tyconName [] (AppT ListT (ConT (genTyconName itemDefId)))]
      other ->
        let itemDefId = mkDefinitionId $ unDefinitionId defId <> "_Item"
         in pure [TySynD tyconName [] (AppT ListT (ConT (genTyconName itemDefId)))] <> decTypes itemDefId other
    ListItemSchemaSchemas schemas ->
      case traverse schemaRefName schemas of
        Left err -> error $ moduleName <> ": " <> err <> " " <> createIssue
        Right tys ->
          case tys of
            [] -> error $ moduleName <> ": tuple schema must contain at least one element."
            [ty] -> pure [TySynD tyconName [] (ConT ty)]
            _ ->
              let tupleTy = foldl AppT (TupleT (length tys)) (map ConT tys)
               in pure [TySynD tyconName [] tupleTy]
  SchemaMap _ MkMapSchema {..} ->
    let
      keyDefId = mkDefinitionId $ unDefinitionId defId <> "_Key"
      valDefId = mkDefinitionId $ unDefinitionId defId <> "_Value"
     in
      pure [TySynD tyconName [] (AppT (AppT (ConT ''PlutusTx.Map) (ConT (genTyconName keyDefId))) (ConT (genTyconName valDefId)))]
        <> decTypes keyDefId msKeys
        <> decTypes valDefId msValues
  SchemaConstructor _ MkConstructorSchema {..} ->
    if csFields == mempty
      then error (moduleName <> ": top level \"constructor\" fields must not be empty. " <> createIssue)
      else pure [DataD [] tyconName [] Nothing [NormalC tyconName (reverse $ foldl' getFieldRefs [] csFields)] stockDerivations]
  SchemaBuiltInData _ -> pure [TySynD tyconName [] (ConT ''PlutusTx.BuiltinData)]
  SchemaBuiltInUnit _ -> errBuiltin "#unit"
  SchemaBuiltInBoolean _ -> errBuiltin "#boolean"
  SchemaBuiltInInteger _ -> errBuiltin "#integer"
  SchemaBuiltInBytes _ -> errBuiltin "#bytes"
  SchemaBuiltInString _ -> errBuiltin "#string"
  SchemaBuiltInPair _ _ -> errBuiltin "#pair"
  SchemaBuiltInList _ _ -> errBuiltin "#list"
  SchemaOneOf ss -> g ss
  SchemaAnyOf ss -> g ss
  SchemaAllOf _ -> error $ moduleName <> ": \"allOf\" is not supported as type is ambiguous."
  SchemaNot _ -> error $ moduleName <> ": \"not\" is not supported as type is ambiguous."
  SchemaDefinitionRef r -> pure [TySynD tyconName [] (ConT (genTyconName r))]
 where
  errBuiltin name = error $ moduleName <> ": \"" <> name <> "\" is a built-in type which are not supported."
  tyconName = genTyconName defId

  schemaRefName :: Schema -> Either String Name
  schemaRefName = \case
    SchemaDefinitionRef d -> Right (genTyconName d)
    other -> Left $ "Unsupported schema inside tuple: " <> show other

  getFieldRefs racc (SchemaDefinitionRef d) = (Bang NoSourceUnpackedness NoSourceStrictness, ConT (genTyconName d)) : racc
  getFieldRefs _ _ = error $ moduleName <> ": \"constructor\" fields must be all of type \"$ref\". " <> createIssue

  g ss =
    let
      compareConstructors (SchemaConstructor _ a) (SchemaConstructor _ b) = compare (csIndex a) (csIndex b)
      compareConstructors _ _ = error $ moduleName <> ": schemas inside \"oneOf\" or \"anyOf\" must be all of dataType \"constructor\". " <> createIssue
      ssSorted = NE.sortBy compareConstructors ss
      f acc s = case s of
        SchemaConstructor schemaInfo MkConstructorSchema {..} ->
          let constructorName = genTyconName $ mkDefinitionId $ unDefinitionId defId <> sanitize (Text.pack (show csIndex) <> fromMaybe "" (title schemaInfo))
           in NormalC constructorName (reverse $ foldl' getFieldRefs [] csFields) : acc
        _ -> error $ moduleName <> ": absurd case encountered when handling constructors."
     in
      pure [DataD [] tyconName [] Nothing (reverse $ foldl' f [] ssSorted) stockDerivations]

type UPLCProgram = UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()

applyConstant :: UPLCProgram -> PLC.Some (PLC.ValueOf UPLC.DefaultUni) -> UPLCProgram
applyConstant (UPLC.Program () v f) c = UPLC.Program () v . UPLC.Apply () f $ UPLC.Constant () c

applyParam :: PlutusTx.ToData p => UPLCProgram -> p -> UPLCProgram
applyParam prog arg = prog `applyConstant` PLC.someValue (PlutusTx.toData arg)

applyParam' :: PlutusTx.ToData p => SBS.ShortByteString -> p -> SBS.ShortByteString
applyParam' (uncheckedDeserialiseUPLC -> prog) = serialiseUPLC . applyParam prog

makeBPTypes' :: FilePath -> Q ([Dec], ContractBlueprint)
makeBPTypes' fp = do
  bp :: ContractBlueprint <- runIO (readJSON fp)
  addDependentFile fp
  decs <- Map.foldlWithKey' (\acc defId schema -> acc <> decTypes defId schema) (pure []) (contractDefinitions bp)
  pure (decs, bp)

makeBPTypes :: FilePath -> Q [Dec]
makeBPTypes fp = fst <$> makeBPTypes' fp

uponBPTypes :: FilePath -> Q [Dec]
uponBPTypes fp = do
  (typeDecs, bp) <- makeBPTypes' fp
  valDecs <-
    foldM
      ( \mapAcc MkValidatorBlueprint {..} -> do
          let
            valName' = Text.unpack $ "applyParamsToBPValidator_" <> sanitize validatorTitle
            valName = mkName valName'
            valUPLC = case compiledValidatorCode <$> validatorCompiled of
              Nothing -> error $ moduleName <> ": " <> valName' <> " is missing compiled code."
              Just c ->
                Text.encodeUtf8 c & BS16.decode & \case
                  Left e -> error $ moduleName <> ": " <> valName' <> " failed to decode compiled code: " <> e
                  Right bs -> SBS.toShort bs
            paramNames =
              zipWith
                ( curry
                    ( \(MkParameterBlueprint {..}, i :: Natural) ->
                        let prefix = valName' <> show i
                         in mkName $ case parameterTitle of
                              Nothing -> prefix
                              Just pt -> prefix <> Text.unpack (sanitize pt)
                    )
                )
                validatorParameters
                [0 ..]
            paramSchemas =
              map
                ( \p -> case parameterSchema p of
                    SchemaDefinitionRef d -> genTyconName d
                    _ -> error $ moduleName <> ": " <> valName' <> " parameter schemas must be of type \"$ref\". " <> createIssue
                )
                validatorParameters
          body <- foldl' (\acc var -> [|applyParam' $acc $(varE var)|]) [|valUPLC|] paramNames
          pure mapAcc
            <> pure [SigD valName (foldr (AppT . AppT ArrowT . ConT) (ConT ''SBS.ShortByteString) paramSchemas)]
            <> pure [FunD valName [Clause (map VarP paramNames) (NormalB body) []]]
      )
      mempty
      (contractValidators bp)
  let
    plcVersion = preamblePlutusVersion $ contractPreamble bp
    plcVersionN = case plcVersion of
      PlutusV1 -> 'PlutusV1
      PlutusV2 -> 'PlutusV2
      PlutusV3 -> 'PlutusV3
    getScript = mkName "scriptFromBPSerialisedScript"
    scriptParamName = mkName "s"
  bodyGetScript <- [|scriptFromSerialisedScript $(varE scriptParamName)|]
  pure [SigD getScript (AppT (AppT ArrowT (ConT ''SBS.ShortByteString)) (AppT (ConT ''GYScript) (PromotedT plcVersionN)))]
    <> pure [FunD getScript [Clause [VarP scriptParamName] (NormalB bodyGetScript) []]]
    <> pure valDecs
    <> foldl' f (pure []) typeDecs
 where
  f acc dec = case dec of
    DataD _ n _ _ _ _ -> acc <> PlutusTx.unstableMakeIsData n
    _ -> acc
