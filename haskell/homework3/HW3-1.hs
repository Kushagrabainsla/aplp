{- |
Module      : Main
Description : A single-file implementation of the IMP language interpreter.
Maintainer  : Kushagra Bainsla
Date        : March 24, 2026

This module provides a complete pipeline for the IMP language:
1. Abstract Syntax Tree (AST) definitions.
2. An Either-monad based interpreter with error handling and short-circuiting.
3. A Parsec-based parser for the IMP concrete syntax.
4. A CLI driver to run .imp files.
-}

module Main where

import           Control.Monad                  (void)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           System.Environment             (getArgs)
import           Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------
-- 1. Types and Data Structures
--------------------------------------------------------------------------------

type Variable = String
type Store    = Map Variable Value
type Result a = Either String a

-- | Abstract Syntax Tree for IMP expressions
data Expression
  = Var Variable                       -- ^ Variable reference: x
  | Val Value                          -- ^ Literal value: 5, true
  | Assign Variable Expression         -- ^ Assignment: x := e
  | Sequence Expression Expression     -- ^ Sequencing: e1; e2
  | Op Binop Expression Expression     -- ^ Binary operations
  | If Expression Expression Expression -- ^ Conditionals: if e1 then e2 else e3 end
  | While Expression Expression        -- ^ Loops: while e1 do e2 end
  | Skip                               -- ^ No-op
  deriving (Show)

-- | Binary operators supported by the language
data Binop
  = Plus | Minus | Times | Divide      -- ^ Arithmetic
  | Gt | Ge | Lt | Le | Eq             -- ^ Comparison
  | And | Or                           -- ^ Boolean Logic
  | Not                                -- ^ Unary Not (encoded as Binop for simplicity)
  deriving (Show)

-- | Primitive values in the IMP language
data Value
  = IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. Interpreter Logic
--------------------------------------------------------------------------------

-- | Entry point for interpreting an expression
run :: Expression -> Result (Value, Store)
run expr = evaluate expr Map.empty

-- | Core evaluation function using the Either monad for error propagation
evaluate :: Expression -> Store -> Result (Value, Store)
evaluate expr store = case expr of
  Var x ->
    case Map.lookup x store of
      Just val -> Right (val, store)
      Nothing  -> Left $ "Runtime Error: Unbound variable '" ++ x ++ "'"

  Val v -> Right (v, store)

  Skip  -> Right (BoolVal False, store)

  Assign x e -> do
    (val, nextStore) <- evaluate e store
    Right (val, Map.insert x val nextStore)

  Sequence e1 e2 -> do
    (_, nextStore) <- evaluate e1 store
    evaluate e2 nextStore

  -- Short-circuiting Boolean Operators
  Op And e1 e2 -> evaluateBool e1 store $ \b1 s1 ->
    if b1 then evaluate e2 s1 else Right (BoolVal False, s1)

  Op Or e1 e2 -> evaluateBool e1 store $ \b1 s1 ->
    if b1 then Right (BoolVal True, s1) else evaluate e2 s1

  Op Not e1 _ -> evaluateBool e1 store $ \b1 s1 ->
    Right (BoolVal (not b1), s1)

  -- Generic Binary Operators (Arithmetic & Comparison)
  Op op e1 e2 -> do
    (v1, s1) <- evaluate e1 store
    (v2, s2) <- evaluate e2 s1
    res      <- applyOp op v1 v2
    Right (res, s2)

  If cond thenBr elseBr -> evaluateBool cond store $ \b s ->
    evaluate (if b then thenBr else elseBr) s

  While cond body -> evaluateBool cond store $ \b s ->
    if b
      then do (_, sNext) <- evaluate body s
              evaluate (While cond body) sNext
      else Right (BoolVal False, s)

-- | Helper to ensure an expression evaluates to a boolean
evaluateBool :: Expression -> Store -> (Bool -> Store -> Result (Value, Store)) -> Result (Value, Store)
evaluateBool expr store continuation = do
  (val, nextStore) <- evaluate expr store
  case val of
    BoolVal b -> continuation b nextStore
    _         -> Left $ "Runtime Error: Expected boolean but got " ++ show val

-- | Implementation of operator logic
applyOp :: Binop -> Value -> Value -> Result Value
applyOp op v1 v2 = case (op, v1, v2) of
  (Plus,   IntVal i, IntVal j) -> Right $ IntVal $ i + j
  (Minus,  IntVal i, IntVal j) -> Right $ IntVal $ i - j
  (Times,  IntVal i, IntVal j) -> Right $ IntVal $ i * j
  (Divide, IntVal _, IntVal 0) -> Left "Runtime Error: Division by zero"
  (Divide, IntVal i, IntVal j) -> Right $ IntVal $ i `div` j
  (Gt,     IntVal i, IntVal j) -> Right $ BoolVal $ i > j
  (Ge,     IntVal i, IntVal j) -> Right $ BoolVal $ i >= j
  (Lt,     IntVal i, IntVal j) -> Right $ BoolVal $ i < j
  (Le,     IntVal i, IntVal j) -> Right $ BoolVal $ i <= j
  (Eq,     IntVal i, IntVal j) -> Right $ BoolVal $ i == j
  (Eq,     BoolVal b1, BoolVal b2) -> Right $ BoolVal $ b1 == b2
  _ -> Left $ "Runtime Error: Invalid operands for " ++ show op ++ ": " ++ show v1 ++ ", " ++ show v2

--------------------------------------------------------------------------------
-- 3. Parser Logic
--------------------------------------------------------------------------------

-- | Parse an IMP file from disk
parseFile :: String -> IO (Either ParseError Expression)
parseFile = parseFromFile (spaces *> expression <* eof)

-- | Main expression parser (handles sequences)
expression :: Parser Expression
expression = do
  e1 <- singleExpr
  (spaces *> char ';' *> spaces *> (Sequence e1 <$> expression)) <|> return e1

-- | Parsers for individual language constructs
singleExpr :: Parser Expression
singleExpr = choice [ try assignment
                    , try ifExpr
                    , try whileExpr
                    , orExpr
                    ]

assignment :: Parser Expression
assignment = do
  var <- identifier
  void $ spaces *> string ":=" <* spaces
  Assign var <$> singleExpr

ifExpr :: Parser Expression
ifExpr = do
  keyword "if"
  cond <- expression
  keyword "then"
  thenBr <- expression
  keyword "else"
  elseBr <- expression
  keyword "end"
  return $ If cond thenBr elseBr

whileExpr :: Parser Expression
whileExpr = do
  keyword "while"
  cond <- expression
  keyword "do"
  body <- expression
  keyword "end"
  return $ While cond body

-- | Operator Precedence Hierarchy
orExpr, andExpr, notExpr, compExpr, addExpr, mulExpr, atom :: Parser Expression

orExpr   = chainl1 andExpr (reservedOp "or"  (Op Or))
andExpr  = chainl1 notExpr (reservedOp "and" (Op And))

notExpr  = (keyword "not" *> ((\e -> Op Not e (Val $ BoolVal False)) <$> notExpr))
       <|> compExpr

compExpr = do
  e1 <- addExpr
  option e1 $ do
    op <- choice [ try (string ">=") *> return Ge
                 , try (string "<=") *> return Le
                 , try (string "==") *> return Eq
                 , try (string ">")  *> return Gt
                 , try (string "<")  *> return Lt
                 ]
    spaces
    Op op e1 <$> addExpr

addExpr  = chainl1 mulExpr (choice [ char '+' *> return (Op Plus), char '-' *> return (Op Minus) ] <* spaces)
mulExpr  = chainl1 atom    (choice [ char '*' *> return (Op Times), char '/' *> return (Op Divide) ] <* spaces)

atom = choice [ between (char '(' <* spaces) (char ')' <* spaces) expression
              , keyword "skip"  *> return Skip
              , keyword "true"  *> return (Val $ BoolVal True)
              , keyword "false" *> return (Val $ BoolVal False)
              , Val . IntVal    <$> intLiteral
              , Var             <$> identifier
              ]

-- | Low-level Parsing Helpers
identifier :: Parser String
identifier = do
  name <- (:) <$> letter <*> many (alphaNum <|> char '_')
  if name `elem` reservedNames
    then unexpected $ "reserved word " ++ name
    else spaces *> return name

intLiteral :: Parser Int
intLiteral = do
  s <- option "" (string "-")
  d <- many1 digit <* spaces
  return $ read (s ++ d)

keyword :: String -> Parser ()
keyword kw = void $ try (string kw <* notFollowedBy alphaNum) <* spaces

reservedOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
reservedOp op constructor = keyword op *> return constructor

reservedNames :: [String]
reservedNames = ["if", "then", "else", "end", "while", "do", "true", "false", "skip", "and", "or", "not"]

--------------------------------------------------------------------------------
-- 4. Main Driver
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runProgram filename
    _          -> putStrLn "Usage: runhaskell HW3.hs <filename.imp>"

runProgram :: String -> IO ()
runProgram filename = do
  parsed <- parseFile filename
  case parsed of
    Left  err  -> putStrLn "Parse error:" >> print err
    Right expr -> do
      putStrLn $ "Parsed expression: " ++ show expr
      putStrLn ""
      case run expr of
        Left  runtimeErr -> putStrLn runtimeErr
        Right (value, store) -> do
          putStrLn $ "Final value: " ++ show value
          putStrLn "Final store:"
          mapM_ printBinding (Map.toList store)

printBinding :: (String, Value) -> IO ()
printBinding (var, val) = putStrLn $ "  " ++ var ++ " = " ++ showVal val
  where
    showVal (IntVal n)  = show n
    showVal (BoolVal b) = if b then "true" else "false"
