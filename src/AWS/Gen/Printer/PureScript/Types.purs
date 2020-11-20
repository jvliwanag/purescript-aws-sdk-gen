module AWS.Gen.Printer.PureScript.Types
       ( addTypesModule
       ) where

import Prelude

import AWS.Gen.Model (MemberType(..), ScalarType(..), ServiceDef, ShapeDef, ShapeType(..), StructureMember, scalarTypeToPSType)
import AWS.Gen.Printer.PureScript.Comment (comment)
import AWS.Gen.Printer.PureScript.Defs (typ_Array, typ_Boolean, typ_Int, typ_Number, typ_Object, typ_String, typ_Timestamp, typ_UndefinedOr, typesModuleName)
import CST.Simple (ModuleBuilder, Type, typCons, typRecord_)
import CST.Simple.ModuleBuilder (addType)
import CST.Simple.ProjectBuilder (ProjectBuilder, addModule)
import Data.Array (null, partition)
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(..), joinWith, replace, replaceAll)
import Data.Tuple.Nested ((/\))

addTypesModule :: ServiceDef -> ProjectBuilder Unit
addTypesModule svcDef = addModule (typesModuleName svcDef) do
  traverse_ addShape svcDef.shapes

addShape :: ShapeDef -> ModuleBuilder Unit
addShape shape = do
  addType
    { export: true
    , name: shape.name
    , typeVarBindings: []
    , type_: shapeToType shape
    }

shapeToType :: ShapeDef -> Type
shapeToType shape = case shape.shapeType of
  STStructure { members } ->
    typRecord_ (toEntry <$> members)
    where
      toEntry m = m.name /\ structurMemberToType m

structurMemberToType :: StructureMember -> Type
structurMemberToType { isRequired, memberType } =
  if isRequired
  then memberTypeToType memberType
  else typ_UndefinedOr $ memberTypeToType memberType

memberTypeToType :: MemberType -> Type
memberTypeToType (MTScalar sc) = scalarTypeToType (sc)
memberTypeToType (MTRef name) = typCons name
memberTypeToType (MTList mt) = typ_Array (memberTypeToType mt)
memberTypeToType (MTMap mt) = typ_Object (memberTypeToType mt)

scalarTypeToType :: ScalarType -> Type
scalarTypeToType SCString = typ_String
scalarTypeToType SCInt = typ_Int
scalarTypeToType SCNumber = typ_Number
scalarTypeToType SCBoolean = typ_Boolean
scalarTypeToType SCTimestamp = typ_Timestamp

{-
structureMemberToRecordTypeEntry :: StructureMember -> String /\ Type
structureMemberToRecordTypeEntry
-}

fileName :: ServiceDef -> String
fileName { name } = name <> "Types"

output :: ServiceDef -> String
output serviceDef =
    (header serviceDef) <>
    (serviceDef.shapes <#> newType serviceDef # joinWith "")

header :: ServiceDef -> String
header { name } = """
module AWS.{{name}}.Types where

import Prelude
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap) as StrMap

import AWS.Request.Types as Types

options :: Options
options = defaultOptions { unwrapSingleConstructors = true }
""" # replace (Pattern "{{name}}") (Replacement name)

newType :: ServiceDef -> ShapeDef -> String
newType svc shape = """
{{documentation}}
newtype {{name}} = {{name}} {{type}}
derive instance newtype{{name}} :: Newtype {{name}} _
derive instance repGeneric{{name}} :: Generic {{name}} _
instance show{{name}} :: Show {{name}} where show = genericShow
instance decode{{name}} :: Decode {{name}} where decode = genericDecode options
instance encode{{name}} :: Encode {{name}} where encode = genericEncode options
{{defaultConstructor}}
""" # replaceAll (Pattern "{{name}}") (Replacement $ shape.name)
    # replace (Pattern "{{type}}") (Replacement $ recordType shape)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment shape.documentation)
    # replace (Pattern "{{defaultConstructor}}") (Replacement $ defaultConstructor shape)

recordType :: ShapeDef -> String
recordType shape = case shape.shapeType of
    STStructure { members } -> recordRecord members

recordArray :: String -> String
recordArray shape = "(Array {{type}})"
    # replace (Pattern "{{type}}") (Replacement shape)

recordMap :: String -> String
recordMap shape = "(StrMap.StrMap {{value}})"
    # replace (Pattern "{{value}}") (Replacement shape)

recordRecord :: Array StructureMember -> String
recordRecord members = if null members
    then "Types.NoArguments"
    else "\n  { {{fields}}\n  }"
        # replace (Pattern "{{fields}}") (Replacement fields)
            where fields = recordFields members # joinWith "\n  , "

recordFields :: Array StructureMember -> Array String
recordFields members = fields
    where field :: StructureMember -> String
          field sm = "\"{{name}}\" :: {{type}}"
              # replace (Pattern "{{name}}") (Replacement sm.name)
              # replace (Pattern "{{type}}") (Replacement $ structureMemberToPSType sm)

          fields = members <#> field

structureMemberToPSType :: StructureMember -> String
structureMemberToPSType { memberType, isRequired } =
  isRequiredF $ memberF memberType
  where
    isRequiredF n =
      if isRequired
      then n
      else "Maybe (" <> n <> ")"

    memberF m = case m of
      MTScalar sc -> scalarTypeToPSType sc
      MTRef name -> name
      MTList m' -> "Array (" <> memberF m' <> ")"
      MTMap m' -> "StrMap.StrMap (" <> memberF m' <> ")"

defaultConstructor :: ShapeDef -> String
defaultConstructor shape = case shape.shapeType of
    STStructure { members } -> defaultRecordConstructor shape members

defaultRecordConstructor :: ShapeDef -> Array StructureMember -> String
defaultRecordConstructor shape members = if null members
    then "" -- there's already a singleton constructor
    else """
-- | Constructs {{name}} from required parameters
new{{name}} :: {{newTypeSignature}}
new{{name}} {{arguments}} = {{name}} { {{fieldAssignments}} }

-- | Constructs {{name}}'s fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
new{{name}}' :: {{fieldsSignature}}
new{{name}}' {{arguments}} customize = ({{name}} <<< customize) { {{fieldAssignments}} }
""" # replaceAll (Pattern "{{name}}") (Replacement shape.name)
    # replaceAll (Pattern "{{newTypeSignature}}") (Replacement newTypeSignature)
    # replaceAll (Pattern "{{fieldsSignature}}") (Replacement fieldsSignature)
    # replaceAll (Pattern "{{arguments}}") (Replacement arguments)
    # replaceAll (Pattern "{{fieldAssignments}}") (Replacement fieldAssignments)
        where newTypeSignature = (signatureTypes <> [shape.name]) # joinWith " -> "
              fieldsSignature = (signatureTypes <> [ "( { " <> fields <> " } -> {" <> fields <> " } )", shape.name]) # joinWith " -> "
              fields = recordFields members # joinWith " , "
              arguments = (escapeArgument <<< _.name) <$> requiredFields # joinWith " "
              fieldAssignments = (requiredFieldAssignments <> optionalFieldAssignments) # joinWith ", "
              requiredFieldAssignments = (\f -> escapeFieldName f.name <> ": " <> escapeArgument f.name) <$> requiredFields
              optionalFieldAssignments = (\f -> escapeFieldName f.name <> ": Nothing") <$> optionalFields
              filteredFields = members
              signatureTypes = structureMemberToPSType <$> requiredFields
              requiredFields = splitFields.yes
              optionalFields = splitFields.no
              splitFields = partition (_.isRequired) filteredFields
              escapeFieldName n = "\"" <> n <> "\""
              escapeArgument n = "_" <> n
