module AWS.Gen.Printer.PureScript.Defs
       ( serviceName
       , requestsModuleName
       , typesModuleName
       , serviceModuleName
         -- Service Specific
       , typ_svc_Service
       , typ_svc_shape
       , expr_svc_unsafeRequest
         -- General
       , cnst_Coercible
       , expr_AWSservice
       , expr_mapOp
       , expr_unsafeRequest
       , expr_void
       , typ_Aff
       , typ_Array
       , typ_AWSService
       , typ_AWSOptions
       , typ_Boolean
       , typ_Effect
       , typ_Int
       , typ_MethodName
       , typ_Number
       , typ_Object
       , typ_String
       , typ_Timestamp
       , typ_UndefinedOr
       , typ_Unit
       ) where

import Prelude

import AWS.Gen.Model (ServiceDef)
import CST.Simple (Constraint, Expr, Type, cnst2, exprIdent1, exprIdent2, exprIdent3, exprOp, typCons, typCons1)

serviceName :: ServiceDef -> String
serviceName { name } =
  name <> "Service"

requestsModuleName :: ServiceDef -> String
requestsModuleName =
  mkModuleName "Requests"

typesModuleName :: ServiceDef -> String
typesModuleName =
  mkModuleName "Types"

serviceModuleName :: ServiceDef -> String
serviceModuleName =
  mkModuleName "Service"

mkModuleName :: String -> ServiceDef -> String
mkModuleName namePart { name } =
  "AWS." <> name <> "." <> namePart

-- Service Specific

typ_svc_Service :: ServiceDef -> Type
typ_svc_Service svcDef =
  typCons $ serviceModuleName svcDef <> "(" <> serviceName svcDef <> ")"

typ_svc_shape :: ServiceDef -> String -> Type
typ_svc_shape svcDef shapeName =
  typCons $ typesModuleName svcDef <> "(" <> shapeName <> ")"

expr_svc_unsafeRequest :: ServiceDef -> Expr -> Expr -> Expr -> Expr
expr_svc_unsafeRequest svcDef =
  exprIdent3 (serviceModuleName svcDef <> "(unsafeRequest)")


-- General

cnst_Coercible :: Type -> Type -> Constraint
cnst_Coercible =
  cnst2 "Untagged.Coercible(class Coercible)"

expr_AWSservice :: Expr -> Expr -> Expr
expr_AWSservice =
  exprIdent2 "AWS.Service(service)"

expr_mapOp :: Expr -> Expr -> Expr
expr_mapOp e1 e2 =
  exprOp e1 "Prelude((<$>))" e2

expr_unsafeRequest :: Expr -> Expr -> Expr -> Expr
expr_unsafeRequest =
  exprIdent3 "AWS.Request(unsafeRequest) as AWS"

expr_void :: Expr -> Expr
expr_void =
  exprIdent1 "Prelude(void)"

typ_Aff :: Type -> Type
typ_Aff =
  typCons1 "Effect.Aff(Aff)"

typ_Array :: Type -> Type
typ_Array =
  typCons1 "Array"

typ_AWSService :: Type
typ_AWSService =
  typCons "AWS.Service(Service)"

typ_AWSOptions :: Type
typ_AWSOptions =
  typCons "AWS.Service(Options)"

typ_Boolean :: Type
typ_Boolean =
  typCons "Boolean"

typ_Effect :: Type -> Type
typ_Effect =
  typCons1 "Effect(Effect)"

typ_Int :: Type
typ_Int =
  typCons "Int"

typ_MethodName :: Type
typ_MethodName =
  typCons "AWS.Request(MethodName)"

typ_Number :: Type
typ_Number =
  typCons "Number"

typ_Object :: Type -> Type
typ_Object =
  typCons1 "Foreign.Object(Object) as FO"

typ_String :: Type
typ_String =
  typCons "String"

typ_Timestamp :: Type
typ_Timestamp =
  typCons "AWS.Request.Types(Timestamp)"

typ_UndefinedOr :: Type -> Type
typ_UndefinedOr =
  typCons1 "Untagged.Union(UndefinedOr)"

typ_Unit :: Type
typ_Unit =
  typCons "Prelude(Unit)"
