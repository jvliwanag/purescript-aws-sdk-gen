module AWS.Gen.Printer.PureScript.Requests
       ( addRequestsModule
       ) where

import Prelude

import AWS.Gen.Model (ServiceDef, OperationDef)
import AWS.Gen.Printer.PureScript.Defs (cnst_Coercible, expr_svc_unsafeRequest, requestsModuleName, typ_Aff, typ_Unit, typ_svc_Service, typ_svc_shape)
import CST.Simple (ModuleBuilder, bndrVar, exprIdent, exprRecord, exprString, tvb, typForall, typVar, (*->), (*=>))
import CST.Simple.ModuleBuilder (addValue)
import CST.Simple.ProjectBuilder (ProjectBuilder, addModule)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Extra (camelCase)
import Data.Traversable (traverse_)

addRequestsModule :: ServiceDef -> ProjectBuilder Unit
addRequestsModule svcDef = addModule (requestsModuleName svcDef) do
  traverse_ (addOperation svcDef) svcDef.operations

addOperation :: ServiceDef -> OperationDef -> ModuleBuilder Unit
addOperation svc op@{ input: Just input } =
  addOperationWithInput svc op input
addOperation svc op =
  addOperationWithoutInput svc op

addOperationWithInput :: ServiceDef -> OperationDef -> String -> ModuleBuilder Unit
addOperationWithInput svcDef { methodName, output: output' } input = do
  addValue
    { export: true
    , name: camelCase methodName
    , binders: [ bndrVar "svc", bndrVar "r" ]
    , type_:
      typForall [ tvb "r" ] $
      cnst_Coercible (typVar "r") (typ_svc_shape svcDef input) *=>
      typ_svc_Service svcDef *->
      typVar "r" *->
      typ_Aff outputType
    , expr:
      expr_svc_unsafeRequest svcDef (exprIdent "svc") (exprString methodName) (exprIdent "r")
    }

    where
      outputType = fromMaybe typ_Unit (typ_svc_shape svcDef <$> output')

addOperationWithoutInput :: ServiceDef -> OperationDef -> ModuleBuilder Unit
addOperationWithoutInput svcDef { methodName, output: output' } = do
  addValue
    { export: true
    , name: camelCase methodName
    , binders: [ bndrVar "svc" ]
    , type_:
      typ_svc_Service svcDef *->
      typ_Aff outputType
    , expr:
      expr_svc_unsafeRequest svcDef (exprIdent "svc") (exprString methodName) (exprRecord [])
    }

  where
    outputType = fromMaybe typ_Unit (typ_svc_shape svcDef <$> output')
