{-
  Name: Kushagra Bainsla
  Class: CS 252
  Assigment: HW3
  Date: March 24, 2026
  Description: IMP language interpreter with Either monad error handling
-}

module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  run,
  runFile
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
type Store = Map Variable Value

-- Result type using Either for error handling
type Result a = Either String a

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  | Skip                                    -- skip (no-op)
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  | Eq       -- == :: Int -> Int -> Bool (or Bool -> Bool -> Bool)
  | And      -- and :: Bool -> Bool -> Bool
  | Or       -- or :: Bool -> Bool -> Bool
  | Not      -- not :: Bool -> Bool (unary, second arg ignored)
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)


-- Apply a binary operator to two values, returning Either for errors
applyOp :: Binop -> Value -> Value -> Result Value
applyOp Plus (IntVal i) (IntVal j) = Right $ IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = Right $ IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = Right $ IntVal $ i * j
applyOp Divide (IntVal _) (IntVal 0) = Left "Error: Division by zero"
applyOp Divide (IntVal i) (IntVal j) = Right $ IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = Right $ BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = Right $ BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = Right $ BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = Right $ BoolVal $ i <= j
applyOp Eq (IntVal i) (IntVal j) = Right $ BoolVal $ i == j
applyOp Eq (BoolVal b1) (BoolVal b2) = Right $ BoolVal $ b1 == b2
-- Boolean operators
applyOp And (BoolVal b1) (BoolVal b2) = Right $ BoolVal $ b1 && b2
applyOp Or (BoolVal b1) (BoolVal b2) = Right $ BoolVal $ b1 || b2
applyOp Not (BoolVal b) _ = Right $ BoolVal $ not b
applyOp op v1 v2 = Left $ "Error: Invalid operands for operator " ++ show op ++
                          ": " ++ show v1 ++ " and " ++ show v2


-- Evaluate an expression using the Either monad
evaluate :: Expression -> Store -> Result (Value, Store)
evaluate (Var x) s = case Map.lookup x s of
  Just v  -> Right (v, s)
  Nothing -> Left $ "Error: Unbound variable '" ++ x ++ "'"

evaluate (Val v) s = Right (v, s)

evaluate Skip s = Right (BoolVal False, s)  -- skip is equivalent to false

evaluate (Assign x e) s = do
  (v, s') <- evaluate e s
  Right (v, Map.insert x v s')

evaluate (Sequence e1 e2) s = do
  (_, s') <- evaluate e1 s
  evaluate e2 s'

-- Boolean short-circuit ops must come BEFORE the generic Op case
evaluate (Op And e1 e2) s = do
  (v1, s1) <- evaluate e1 s
  case v1 of
    BoolVal True  -> evaluate e2 s1
    BoolVal False -> Right (BoolVal False, s1)
    _             -> Left "Error: Operand for 'and' must be boolean"

evaluate (Op Or e1 e2) s = do
  (v1, s1) <- evaluate e1 s
  case v1 of
    BoolVal True  -> Right (BoolVal True, s1)
    BoolVal False -> evaluate e2 s1
    _             -> Left "Error: Operand for 'or' must be boolean"

evaluate (Op Not e1 _) s = do
  (v1, s1) <- evaluate e1 s
  case v1 of
    BoolVal b -> Right (BoolVal (not b), s1)
    _         -> Left "Error: Operand for 'not' must be boolean"

-- Generic binary op (arithmetic and comparisons)
evaluate (Op o e1 e2) s = do
  (v1, s1) <- evaluate e1 s
  (v2, s2) <- evaluate e2 s1
  result <- applyOp o v1 v2
  Right (result, s2)

evaluate (If e1 e2 e3) s = do
  (v1, s1) <- evaluate e1 s
  case v1 of
    BoolVal True  -> evaluate e2 s1
    BoolVal False -> evaluate e3 s1
    _             -> Left "Error: Condition in 'if' must be boolean"

evaluate (While e1 e2) s = do
  (v1, s1) <- evaluate e1 s
  case v1 of
    BoolVal True  -> do
      (_, s2) <- evaluate e2 s1
      evaluate (While e1 e2) s2
    BoolVal False -> Right (BoolVal False, s1)
    _             -> Left "Error: Condition in 'while' must be boolean"


-- Evaluates a program with an initially empty state
run :: Expression -> Result (Value, Store)
run prog = evaluate prog Map.empty

-- Run and return just the final Store, or an error message
runFile :: Expression -> Either String Store
runFile prog = case run prog of
  Right (_, store) -> Right store
  Left err         -> Left err
