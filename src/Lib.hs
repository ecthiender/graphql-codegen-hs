{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.GraphQL.Draft.Parser as GQL
import qualified Language.GraphQL.Draft.Syntax as GQL
import qualified System.Exit as Sys
import Text.Pretty.Simple
import Control.Arrow ((&&&))
import Data.List (find)

-- Source AST is GQL.SchemaDocument

-- | the concrete types we are interested in
type ObjectTypeDef = GQL.ObjectTypeDefinition GQL.InputValueDefinition
type InputTypeDef = GQL.InputObjectTypeDefinition GQL.InputValueDefinition

data InterestingTypes = ITInput InputTypeDef | ITObject ObjectTypeDef

-- | Target AST for TS interfaces
data TSInterface
  = TSInterface
  { tsiName :: Text
  , tsiFields :: [TSInterfaceField]
  } deriving (Show, Eq)

data TSInterfaceField
  = TSInterfaceField
  { tsifName :: Text
  , tsifType :: TSType
  } deriving (Show, Eq)

data TSType
  = TSTNamed Text
  | TSTList TSType
  deriving (Show, Eq)

generateCode :: GQL.SchemaDocument -> String
generateCode schema =
  let allTypes = getInterestingTypes schema roots
      roots = getRoots schema
      ts = map mapToTSInterface allTypes
      interfaces = T.stripEnd $ T.unlines $ map serializeTSInterface ts
  in T.unpack interfaces

-- | Given a 'SchemaDocument', returns the name of the root query object. This
-- function right now is a bit unwieldly. Is there a more obvious way to walk
-- down the AST?
getRoots :: GQL.SchemaDocument -> [GQL.Name]
getRoots (GQL.SchemaDocument tydefs) =
  case concatMap getRootName tydefs of
    [] -> [GQL.unsafeMkName "Query"]
    roots -> roots
  where
    getRootName = \case
      GQL.TypeSystemDefinitionSchema sd ->
        let rootOps = filter (\x -> GQL._rotdOperationType x `elem` rootOperations)
                     $ GQL._sdRootOperationTypeDefinitions sd
        in GQL._rotdOperationTypeType <$> rootOps
      _ -> []

rootOperations :: [GQL.OperationType]
rootOperations = [ GQL.OperationTypeQuery
                 , GQL.OperationTypeMutation
                 , GQL.OperationTypeSubscription
                 ]

parseFile :: FilePath -> IO GQL.SchemaDocument
parseFile filepath = do
  schemaDoc <- readFile filepath
  let res = GQL.parseSchemaDocument $ T.pack schemaDoc
  case res of
    Right r -> pure r
    Left e -> do
      putStrLn $ "ERROR: Failed parsing GraphQL schema: " <> T.unpack e
      Sys.exitFailure

-- | Get all object and input types from the schema. (ignoring the root object)
getInterestingTypes :: GQL.SchemaDocument -> [GQL.Name] -> [InterestingTypes]
getInterestingTypes (GQL.SchemaDocument typeSysDefs) rootNames =
  mapMaybe getObjectDef typeSysDefs
  where
    getObjectDef = \case
      GQL.TypeSystemDefinitionSchema _ -> Nothing
      GQL.TypeSystemDefinitionType typeDef -> case typeDef of
        GQL.TypeDefinitionObject objDef ->
          if GQL._otdName objDef `elem` rootNames
          then Nothing
          else Just $ ITObject objDef
        GQL.TypeDefinitionInputObject inpDef -> Just $ ITInput inpDef
        _ -> Nothing

mapToTSInterface :: InterestingTypes -> TSInterface
mapToTSInterface obj =
  let name = getName obj
      fields = getFields obj
      tsFields = map (\(name, gtype) -> TSInterfaceField (GQL.unName name) (getTypeName gtype)) fields
  in TSInterface name tsFields
  where
    getName = GQL.unName . \case
      ITInput i ->  GQL._iotdName i
      ITObject o -> GQL._otdName o
    getFields = \case
      ITInput i -> map (GQL._ivdName &&& GQL._ivdType) $ GQL._iotdValueDefinitions i
      ITObject o -> map (GQL._fldName &&& GQL._fldType) $ GQL._otdFieldsDefinition o
    getTypeName = \case
      GQL.TypeNamed _ name -> case GQL.unName name of
        -- ID is GraphQL primitive, convert that to String. All other primitive
        -- types map with the same name to TS primitives
        "ID" -> TSTNamed "String"
        _    -> TSTNamed $ GQL.unName name
      GQL.TypeList _ typ -> TSTList $ getTypeName typ

serializeTSInterface :: TSInterface -> Text
serializeTSInterface (TSInterface name fields) =
  T.unlines [ "interface " <> name <> " {"
            , T.intercalate "\n" $ map serializeTSInterfaceField fields
            , "}"
            ]

serializeTSInterfaceField :: TSInterfaceField -> Text
serializeTSInterfaceField (TSInterfaceField name type_) =
  "  " <> name <> ": " <> serializeTSType type_ <> ","

serializeTSType :: TSType -> Text
serializeTSType = \case
  TSTNamed t -> t
  TSTList t -> "[" <> serializeTSType t <> "]"
