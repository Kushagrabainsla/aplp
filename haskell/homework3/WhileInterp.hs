{-
  Name: Kushagra Bainsla
  Class: CS 252
  Assigment: HW3
  Date: March 27
  Description: A parser and interpreter for the IMP language using Parsec and the Either Monad.
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  runFile,
  showParsedExp,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except

-- We represent variables as strings.
type Variable = String

--We also represent error messages as strings.
type ErrorMsg = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3 endif
  | While Expression Expression             -- while e1 do e2 endwhile
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
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


fileP :: GenParser Char st Expression
fileP = do
  prog <- exprP
  spaces
  eof
  return prog

exprP = do
  e <- exprP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing   -> e
    Just e' -> Sequence e e')

-- Expressions are divided into terms and expressions for the sake of
-- parsing.  Note that binary operators **DO NOT** follow the expected
-- presidence rules.
--
exprP' = do
  spaces
  try assignmentP <|> compP

assignmentP = do
  v <- varP
  spaces
  string ":="
  spaces
  e <- exprP'
  return $ case v of
    Var x -> Assign x e
    _     -> error "Expected variable for assignment"

compP = do
  l <- addP
  rest <- optionMaybe $ try $ do
    spaces
    op <- try (string "<=") <|> string "<" <|> try (string ">=") <|> string ">"
    spaces
    r <- addP
    return (transOp op, r)
  return $ case rest of
    Nothing -> l
    Just (op, r) -> Op op l r

addP = mulP `chainl1` addOp
  where
    addOp = try $ do
      spaces
      op <- string "+" <|> string "-"
      spaces
      return (\l r -> Op (transOp op) l r)

mulP = termP `chainl1` mulOp
  where
    mulOp = try $ do
      spaces
      op <- string "*" <|> string "/"
      spaces
      return (\l r -> Op (transOp op) l r)

restSeqP :: GenParser Char st Expression
restSeqP = do
  char ';'
  exprP

transOp s = case s of
  "+"  -> Plus
  "-"  -> Minus
  "*"  -> Times
  "/"  -> Divide
  ">=" -> Ge
  ">"  -> Gt
  "<=" -> Le
  "<"  -> Lt
  o    -> error $ "Unexpected operator " ++ o

-- Some string, followed by an expression
restP = do
  ch <- string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> try (string "<=")
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    <|> string ":=" -- not really a binary operator, but it fits in nicely here.
    <?> "binary operator"
  e <- exprP'
  return (ch, e)

-- All terms can be distinguished by looking at the first character
termP = valP
    <|> ifP
    <|> whileP
    <|> parenP
    <|> varP
    <?> "value, variable, 'if', 'while', or '('"


valP = do
  v <- boolP <|> numberP
  return $ Val v

boolP = do
  bStr <- string "true" <|> string "false" <|> string "skip"
  return $ case bStr of
    "true" -> BoolVal True
    "false" -> BoolVal False
    "skip" -> BoolVal False -- Treating the command 'skip' as a synonym for false, for ease of parsing

numberP = do
  n <- many1 digit
  return $ IntVal (read n)

varP = do
  f <- letter
  r <- many (alphaNum <|> char '_')
  return $ Var (f:r)

ifP = do
  string "if"
  spaces
  e1 <- exprP
  spaces
  string "then"
  spaces
  e2 <- exprP
  spaces
  string "else"
  spaces
  e3 <- exprP
  spaces
  string "endif"
  return $ If e1 e2 e3

whileP = do
  string "while"
  spaces
  e1 <- exprP
  spaces
  string "do"
  spaces
  e2 <- exprP
  spaces
  string "endwhile"
  return $ While e1 e2

parenP = do
  char '('
  spaces
  e <- exprP
  spaces
  char ')'
  return e


-- This function will be useful for defining binary operations.
-- Unlike in the previous assignment, this function returns an "Either value".
-- The right side represents a successful computaton.
-- The left side is an error message indicating a problem with the program.
-- The first case is done for you.
applyOp :: Binop -> Value -> Value -> Either ErrorMsg Value
applyOp Plus (IntVal i) (IntVal j) = Right $ IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = Right $ IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = Right $ IntVal $ i * j
applyOp Divide _ (IntVal 0) = Left "Divide by zero"
applyOp Divide (IntVal i) (IntVal j) = Right $ IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = Right $ BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = Right $ BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = Right $ BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = Right $ BoolVal $ i <= j
applyOp _ _ _ = Left "Invalid operands"


-- As with the applyOp method, the semantics for this function
-- should return Either values.  Left <error msg> indicates an error,
-- whereas Right <something> indicates a successful execution.
evaluate :: Expression -> Store -> Either ErrorMsg (Value, Store)
evaluate (Op o e1 e2) s = do
  (v1,s1) <- evaluate e1 s
  (v2,s') <- evaluate e2 s1
  v <- applyOp o v1 v2
  return (v, s')
evaluate (Var x) s = case Map.lookup x s of
  Just v -> Right (v, s)
  Nothing -> Left $ "Variable " ++ x ++ " not found"
evaluate (Val v) s = Right (v, s)
evaluate (Assign x e) s = do
  (v, s') <- evaluate e s
  Right (v, Map.insert x v s')
evaluate (Sequence e1 e2) s = do
  (_, s') <- evaluate e1 s
  evaluate e2 s'
evaluate (If e1 e2 e3) s = do
  (v, s') <- evaluate e1 s
  case v of
    BoolVal True -> evaluate e2 s'
    BoolVal False -> evaluate e3 s'
    IntVal i -> Left $ "test.hs: Non-boolean value '" ++ show i ++ "' used as a conditional"
evaluate w@(While e1 e2) s = do
  (v, s') <- evaluate e1 s
  case v of
    BoolVal True -> do
      (_, s'') <- evaluate e2 s'
      evaluate w s''
    BoolVal False -> Right (BoolVal False, s')
    IntVal i -> Left $ "test.hs: Non-boolean value '" ++ show i ++ "' used as a conditional"


-- Evaluates a program with an initially empty state
run :: Expression -> Either ErrorMsg (Value, Store)
run prog = evaluate prog Map.empty

showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp

runFile fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> putStrLn msg
        Right (v,s) -> print $ show s


