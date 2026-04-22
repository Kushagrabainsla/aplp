import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

data Expression =
    ETrue
  | EFalse
  | EInt Int
  | ESucc Expression
  | EPred Expression
  | EIsZero Expression
  | EIf Expression Expression Expression
  | ELambda Variable StlcType Expression -- (\x:T.e)
  | EApp Expression Expression
  | EVar Variable
  deriving (Show)

data Value =
    VTrue
  | VFalse
  | VNum Int
  deriving (Show)

data StlcType =
    TBool
  | TInt
  | TFun StlcType StlcType  --  T1 -> T2
  deriving (Show,Eq)

type TypingEnv = Map Variable StlcType

--I'm using zero a lot, so I am making a shortcut
zero = EInt 0

typecheckFail e = error $ "Expression " ++ (show e) ++ " does not typecheck"

typecheck :: Expression -> TypingEnv -> StlcType
typecheck ETrue _ = TBool
typecheck EFalse _ = TBool
typecheck (EInt _) _ = TInt
typecheck expr@(ESucc argExpr) env = case typecheck argExpr env of
  TInt  -> TInt
  _     -> typecheckFail expr
typecheck expr@(EPred argExpr) env = case typecheck argExpr env of
  TInt  -> TInt
  _     -> typecheckFail expr
typecheck expr@(EIf condExpr thenExpr elseExpr) env =
  let condType = typecheck condExpr env
      thenType = typecheck thenExpr env
      elseType = typecheck elseExpr env
  in if condType == TBool && thenType == elseType then thenType else typecheckFail expr
typecheck expr@(EIsZero argExpr) env = case typecheck argExpr env of
  TInt  -> TBool
  _     -> typecheckFail expr
typecheck expr@(EVar varName) env =
  case Map.lookup varName env of
    Just varType -> varType
    Nothing      -> typecheckFail expr
typecheck (ELambda paramName paramType bodyExpr) env =
  let extendedEnv = Map.insert paramName paramType env
      bodyType = typecheck bodyExpr extendedEnv
  in TFun paramType bodyType
typecheck expr@(EApp functionExpr argumentExpr) env =
  let functionType = typecheck functionExpr env
      argumentType = typecheck argumentExpr env
  in case functionType of
       TFun expectedArgType resultType | expectedArgType == argumentType -> resultType
       _                                                                 -> typecheckFail expr


--Some sample cases
test1 = typecheck (ESucc zero) Map.empty
test2 = typecheck (EPred (ESucc zero)) Map.empty
test3 = typecheck (EIf ETrue zero (ESucc (ESucc zero))) Map.empty
test4 = typecheck (ELambda "x" TInt ETrue) Map.empty
test5 = typecheck (EApp (ELambda "x" TInt (EIf (EIsZero (EVar "x")) (ESucc zero) zero)) (ESucc zero)) Map.empty

bad1 = typecheck (ESucc EFalse) Map.empty
bad2 = typecheck (EApp (ELambda "x" TInt (EIsZero (EVar "x"))) ETrue) Map.empty
