{-
  Name: <Your name here>
  Class: CS 252
  Assigment: HW2
  Date: <Date assignment is due>
  Description: <Describe the program and what it does>
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
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
  | And      -- and :: Bool -> Bool -> Bool
  | Or       -- or :: Bool -> Bool -> Bool
  | Not      -- not :: Bool -> Bool -> Bool (unary, second arg ignored)
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal 0) = error "Divide by zero"
applyOp Divide (IntVal i) (IntVal j) = IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = BoolVal $ i <= j
-- Boolean operators
applyOp And (BoolVal b1) (BoolVal b2) = BoolVal $ b1 && b2
applyOp Or (BoolVal b1) (BoolVal b2) = BoolVal $ b1 || b2
applyOp _ _ _ = error "Invalid operands for operator"


-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
evaluate (Var x) s = case Map.lookup x s of
  Just v  -> (v, s)
  Nothing -> error $ "Unbound variable: " ++ x
evaluate (Val v) s = (v, s)
evaluate (Assign x e) s =
  let (v, s') = evaluate e s
  in (v, Map.insert x v s')
evaluate (Sequence e1 e2) s =
  let (_, s')   = evaluate e1 s
      (v2, s'') = evaluate e2 s'
  in (v2, s'')
-- Boolean short-circuit ops must come BEFORE the generic Op case
evaluate (Op And e1 e2) s =
  let (v1, s1) = evaluate e1 s
  in case v1 of
    BoolVal True  -> evaluate e2 s1
    BoolVal False -> (BoolVal False, s1)
    _             -> error "Operand for And must be boolean"
evaluate (Op Or e1 e2) s =
  let (v1, s1) = evaluate e1 s
  in case v1 of
    BoolVal True  -> (BoolVal True, s1)
    BoolVal False -> evaluate e2 s1
    _             -> error "Operand for Or must be boolean"
evaluate (Op Not e1 _) s =
  let (v1, s1) = evaluate e1 s
  in case v1 of
    BoolVal True  -> (BoolVal False, s1)
    BoolVal False -> (BoolVal True, s1)
    _             -> error "Operand for Not must be boolean"
-- Generic binary op (arithmetic and comparisons)
evaluate (Op o e1 e2) s =
  let (v1, s1) = evaluate e1 s
      (v2, s') = evaluate e2 s1
  in (applyOp o v1 v2, s')
evaluate (If e1 e2 e3) s =
  let (v1, s1) = evaluate e1 s
  in case v1 of
    BoolVal True  -> let (v2, s2) = evaluate e2 s1 in (v2, s2)
    BoolVal False -> let (v3, s3) = evaluate e3 s1 in (v3, s3)
    _             -> error "Condition in If must be boolean"
evaluate (While e1 e2) s =
  let (v1, s1) = evaluate e1 s
  in case v1 of
    BoolVal True  -> let (_, s2) = evaluate e2 s1
                         (v, s3) = evaluate (While e1 e2) s2
                     in (v, s3)
    BoolVal False -> (BoolVal False, s1)
    _             -> error "Condition in While must be boolean"
evaluate _ _ = error "TBD"


-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog
