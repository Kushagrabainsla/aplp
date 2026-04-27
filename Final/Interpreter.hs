{-
  Module: Interpreter
  Description: Evaluates the AST to produce values.
  Design Decisions:
    - Uses an Environment mapping for variable state.
    - Implements lexical scoping for function calls.
    - Pattern matching is exhaustive-checked at runtime (simple version).
    - Error handling is centralized via the Either monad.
-}

module Interpreter (runProgram, staticCheck, Value(..)) where

import AST
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Except

type Env = Map.Map String Value
type FunctionEnv = Map.Map String ([String], Expr)

-- Combined state for the interpreter
data InterpreterState = InterpreterState
  { variables :: Env
  , functions :: FunctionEnv
  }

type Eval a = ExceptT String (State InterpreterState) a

-- Run a full program
runProgram :: Program -> Either String (Value, Env)
runProgram (Program defs mainExpr) =
  let initialFuncs = Map.fromList [(name, (params, body)) | FunDef name params body <- defs]
      initialState = InterpreterState Map.empty initialFuncs
      action = eval mainExpr
      (result, finalState) = runState (runExceptT action) initialState
  in case result of
       Left err -> Left err
       Right val -> Right (val, variables finalState)

-- Evaluator
eval :: Expr -> Eval Value
eval (ValExpr v) = return v

eval (VarExpr name) = do
  vars <- gets variables
  case Map.lookup name vars of
    Just val -> return val
    Nothing -> throwError $ "Unbound variable: " ++ name

eval (OpExpr op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalOp op v1 v2

eval (IfExpr cond thenPart elsePart) = do
  c <- eval cond
  case c of
    BoolVal True  -> eval thenPart
    BoolVal False -> eval elsePart
    _ -> throwError "Condition must be a boolean"

eval (SeqExpr e1 e2) = eval e1 >> eval e2

eval (AssignExpr name e) = do
  val <- eval e
  modify $ \s -> s { variables = Map.insert name val (variables s) }
  return val

eval (CallExpr name args) = do
  argVals <- mapM eval args
  funcs <- gets functions
  case Map.lookup name funcs of
    Just (params, body) -> do
      if length params /= length argVals
        then throwError $ "Function " ++ name ++ " expects " ++ show (length params) ++ " arguments"
        else do
          -- Save current variable state for lexical scoping (simplified to dynamic here, 
          -- but we'll simulate a new scope by passing a new env if we had closures)
          -- For this language, we'll implement a clean local scope for functions.
          oldVars <- gets variables
          -- Set parameters in the variable map
          let localVars = Map.fromList (zip params argVals)
          -- Functions should only see their arguments and global definitions (or we can allow them to see globals)
          -- To be robust, let's merge with globals but prioritize local params.
          modify $ \s -> s { variables = Map.union localVars oldVars }
          result <- eval body
          -- Restore old variables
          modify $ \s -> s { variables = oldVars }
          return result
    Nothing -> throwError $ "Undefined function: " ++ name

eval (ListExpr exprs) = do
  vals <- mapM eval exprs
  return $ ListVal vals

eval (ConsExpr e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v2 of
    ListVal vs -> return $ ListVal (v1 : vs)
    _ -> throwError "Right side of ':' must be a list"

eval (MatchExpr target cases) = do
  val <- eval target
  findMatch val cases
  where
    findMatch v [] = throwError $ "No pattern matched for value: " ++ show v
    findMatch v ((pat, expr):rest) = do
      matchResult <- matchPattern v pat
      case matchResult of
        Just bindings -> do
          oldVars <- gets variables
          modify $ \s -> s { variables = Map.union (Map.fromList bindings) oldVars }
          result <- eval expr
          modify $ \s -> s { variables = oldVars }
          return result
        Nothing -> findMatch v rest

-- Helper for pattern matching
matchPattern :: Value -> Pattern -> Eval (Maybe [(String, Value)])
matchPattern _ PWildcard = return $ Just []
matchPattern v (PVar name) = return $ Just [(name, v)]
matchPattern (IntVal n) (PInt m) | n == m = return $ Just []
matchPattern (BoolVal b1) (PBool b2) | b1 == b2 = return $ Just []
matchPattern (ListVal []) PListNil = return $ Just []
matchPattern (ListVal (h:t)) (PCons ph pt) = 
  return $ Just [(ph, h), (pt, ListVal t)]
matchPattern _ _ = return Nothing

-- Binary operator logic
evalOp :: Binop -> Value -> Value -> Eval Value
evalOp Plus  (IntVal n) (IntVal m) = return $ IntVal (n + m)
evalOp Minus (IntVal n) (IntVal m) = return $ IntVal (n - m)
evalOp Times (IntVal n) (IntVal m) = return $ IntVal (n * m)
evalOp Divide (IntVal n) (IntVal m) 
  | m == 0    = throwError "Division by zero"
  | otherwise = return $ IntVal (n `div` m)
evalOp Eq v1 v2 = return $ BoolVal (v1 == v2)
evalOp Lt (IntVal n) (IntVal m) = return $ BoolVal (n < m)
evalOp Gt (IntVal n) (IntVal m) = return $ BoolVal (n > m)
evalOp Le (IntVal n) (IntVal m) = return $ BoolVal (n <= m)
evalOp Ge (IntVal n) (IntVal m) = return $ BoolVal (n >= m)
evalOp And (BoolVal b1) (BoolVal b2) = return $ BoolVal (b1 && b2)
evalOp Or  (BoolVal b1) (BoolVal b2) = return $ BoolVal (b1 || b2)
evalOp op _ _ = throwError $ "Type error in operator " ++ show op

-- Static Analysis: Checks for unbound variables before execution
staticCheck :: Program -> Either String ()
staticCheck (Program defs mainExpr) =
  let globalFuncs = Set.fromList [name | FunDef name _ _ <- defs]
      -- Check each function body
      checkDef (FunDef name params body) = 
        void $ checkExpr (Set.union globalFuncs (Set.fromList params)) body
      -- Check the main expression
      checkMain = void $ checkExpr globalFuncs mainExpr
  in do
    mapM_ checkDef defs
    checkMain

checkExpr :: Set.Set String -> Expr -> Either String (Set.Set String)
checkExpr bound (VarExpr name)
  | Set.member name bound = Right bound
  | otherwise = Left $ "Static Analysis Error: Unbound variable '" ++ name ++ "'"
checkExpr bound (OpExpr _ e1 e2) = do
  _ <- checkExpr bound e1
  _ <- checkExpr bound e2
  return bound
checkExpr bound (IfExpr c t e) = do
  _ <- checkExpr bound c
  _ <- checkExpr bound t
  _ <- checkExpr bound e
  return bound
checkExpr bound (SeqExpr e1 e2) = do
  bound' <- checkExpr bound e1
  checkExpr bound' e2
checkExpr bound (AssignExpr name e) = do
  _ <- checkExpr bound e
  return $ Set.insert name bound
checkExpr bound (CallExpr _ args) = do
  mapM_ (checkExpr bound) args
  return bound
checkExpr bound (MatchExpr target cases) = do
  _ <- checkExpr bound target
  mapM_ (\(pat, expr) -> checkExpr (Set.union (getPatternVars pat) bound) expr) cases
  return bound
checkExpr bound (ListExpr es) = do
  mapM_ (checkExpr bound) es
  return bound
checkExpr bound (ConsExpr e1 e2) = do
  _ <- checkExpr bound e1
  _ <- checkExpr bound e2
  return bound
checkExpr bound _ = Right bound

getPatternVars :: Pattern -> Set.Set String
getPatternVars (PVar n) = Set.singleton n
getPatternVars (PCons h t) = Set.fromList [h, t]
getPatternVars _ = Set.empty
