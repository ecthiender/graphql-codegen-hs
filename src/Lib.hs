{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.GraphQL.Draft.Parser as GQL
import qualified Language.GraphQL.Draft.Syntax as GQL
import qualified System.Exit as Sys
import Text.Pretty.Simple

-- Source AST is GQL.SchemaDocument

-- | the concrete object type we are interested in
type ObjectTypeDef = GQL.ObjectTypeDefinition GQL.InputValueDefinition

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
  let allTypes = getAllObjectTypes schema (GQL.unsafeMkName "Query")
      ts = map mapToTSInterface allTypes
      interfaces = T.stripEnd $ T.unlines $ map serializeTSInterface ts
  in T.unpack interfaces

parseFile :: FilePath -> IO GQL.SchemaDocument
parseFile filepath = do
  schemaDoc <- readFile filepath
  let res = GQL.parseSchemaDocument $ T.pack schemaDoc
  case res of
    Right r -> pure r
    Left e -> do
      putStrLn $ "ERROR: Failed parsing GraphQL schema: " <> T.unpack e
      Sys.exitFailure

-- | get all object types ignoring the root object
getAllObjectTypes :: GQL.SchemaDocument -> GQL.Name -> [ObjectTypeDef]
getAllObjectTypes (GQL.SchemaDocument typeSysDefs) rootName =
  filter ((/= rootName) . GQL._otdName) $ mapMaybe getObjectDef typeSysDefs
  where
    getObjectDef = \case
      GQL.TypeSystemDefinitionSchema _ -> Nothing
      GQL.TypeSystemDefinitionType typeDef -> case typeDef of
        GQL.TypeDefinitionObject objDef -> Just objDef
        _ -> Nothing

mapToTSInterface :: ObjectTypeDef -> TSInterface
mapToTSInterface obj = TSInterface (getName obj) tsFields
  where
    getName = GQL.unName . GQL._otdName
    tsFields = map (\(name, gtype) -> TSInterfaceField (GQL.unName name) (getTypeName gtype)) fields
    fields = map (\x -> (GQL._fldName x, GQL._fldType x)) $ GQL._otdFieldsDefinition obj
    getTypeName = \case
      GQL.TypeNamed _ name -> TSTNamed $ GQL.unName name
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
