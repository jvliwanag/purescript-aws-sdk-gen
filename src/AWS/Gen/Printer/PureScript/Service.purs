module AWS.Gen.Printer.PureScript.Service
       ( addServiceModule
       ) where

import Prelude

import AWS.Gen.Model (ServiceDef)
import AWS.Gen.Printer.PureScript.Defs (cnst_Coercible, expr_AWSservice, expr_mapOp, expr_unsafeRequest, serviceModuleName, serviceName, typ_AWSOptions, typ_AWSService, typ_Aff, typ_Effect, typ_MethodName)
import CST.Simple (bndrConstructor, bndrVar, exprCons, exprIdent, exprString, tvb, typCons, typForall, typVar, (*->), (*=>))
import CST.Simple.ModuleBuilder (addNewtype, addValue)
import CST.Simple.ProjectBuilder (ProjectBuilder, addModule)
import CST.Simple.Types (DataExport(..))
import Data.Maybe (Maybe(..))
import Data.String.Extra (pascalCase)

addServiceModule :: ServiceDef -> ProjectBuilder Unit
addServiceModule svcDef = addModule (serviceModuleName svcDef) do
  addNewtype
    { export: Just DataExportAll
    , name: serviceName'
    , typeVarBindings: []
    , consName: serviceName'
    , type_: typ_AWSService
    }

  addValue
    { export: true
    , name: "new" <> pascalCase serviceName'
    , type_:
      typForall [ tvb "r" ] $
      cnst_Coercible (typVar "r") typ_AWSOptions *=>
      typVar "r" *->
      typ_Effect (typCons serviceName')
    , binders:
      [ bndrVar "r"
      ]
    , expr:
      (exprCons serviceName') `expr_mapOp` (expr_AWSservice (exprString svcDef.name) (exprIdent "r"))
    }

  addValue
    { export: true
    , name: "unsafeRequest"
    , type_:
      typForall [ tvb "input", tvb "output" ] $
      typCons serviceName' *->
      typ_MethodName *->
      typVar "input" *->
      typ_Aff (typVar "output")
    , binders: [ bndrConstructor bndrConsName [ bndrVar "svc" ]
               , bndrVar "methodName"
               , bndrVar "input"
               ]
    , expr:
      expr_unsafeRequest (exprIdent "svc") (exprIdent "methodName") (exprIdent "input")
    }

  where
    serviceName' = serviceName svcDef

    bndrConsName = serviceName' <> "(" <> serviceName' <> ")"
