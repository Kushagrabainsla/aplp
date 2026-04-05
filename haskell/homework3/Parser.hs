{-
  Name: Kushagra Bainsla
  Class: CS 252
  Assigment: HW3
  Date: March 24, 2026
  Description: Parser for the IMP language
-}

module Parser (
  parseFile,
  parseString
) where

import Text.ParserCombinators.Parsec
import WhileInterp

-- Parse an IMP file
parseFile :: String -> IO (Either ParseError Expression)
parseFile filename = parseFromFile impFile filename

-- Parse an IMP string
parseString :: String -> Either ParseError Expression
parseString input = parse impFile "(input)" input

-- Top-level parser
impFile :: GenParser Char st Expression
impFile = do
  spaces
  result <- expression
  spaces
  eof
  return result

-- Expression parser (handles sequences with semicolons)
expression :: GenParser Char st Expression
expression = do
  e1 <- singleExpr
  rest <- optionMaybe (try $ do
    spaces
    char ';'
    spaces
    e2 <- expression
    return e2)
  case rest of
    Nothing -> return e1
    Just e2 -> return $ Sequence e1 e2

-- Single expression (not a sequence)
singleExpr :: GenParser Char st Expression
singleExpr = try assignment
         <|> try ifExpr
         <|> try whileExpr
         <|> orExpr

-- Assignment: variable := expression
assignment :: GenParser Char st Expression
assignment = do
  var <- identifier
  spaces
  string ":="
  spaces
  e <- singleExpr
  return $ Assign var e

-- If expression: if e then e else e end
ifExpr :: GenParser Char st Expression
ifExpr = do
  keyword "if"
  cond <- expression
  keyword "then"
  thenBranch <- expression
  keyword "else"
  elseBranch <- expression
  keyword "end"
  return $ If cond thenBranch elseBranch

-- While expression: while e do e end
whileExpr :: GenParser Char st Expression
whileExpr = do
  keyword "while"
  cond <- expression
  keyword "do"
  body <- expression
  keyword "end"
  return $ While cond body

-- Helper for parsing keywords
keyword :: String -> GenParser Char st ()
keyword kw = do
  spaces
  string kw
  notFollowedBy alphaNum
  spaces

-- Or expression (lowest precedence for binary ops)
orExpr :: GenParser Char st Expression
orExpr = chainl1 andExpr orOp

orOp :: GenParser Char st (Expression -> Expression -> Expression)
orOp = try $ do
  spaces
  string "or"
  notFollowedBy alphaNum
  spaces
  return $ Op Or

-- And expression
andExpr :: GenParser Char st Expression
andExpr = chainl1 notExpr andOp

andOp :: GenParser Char st (Expression -> Expression -> Expression)
andOp = try $ do
  spaces
  string "and"
  notFollowedBy alphaNum
  spaces
  return $ Op And

-- Not expression (unary)
notExpr :: GenParser Char st Expression
notExpr = do
  spaces
  notOp <- optionMaybe (try (string "not" >> notFollowedBy alphaNum >> spaces))
  case notOp of
    Just _ -> do
      e <- notExpr
      return $ Op Not e (Val $ BoolVal False)  -- dummy second argument
    Nothing -> compExpr

-- Comparison expression
compExpr :: GenParser Char st Expression
compExpr = do
  e1 <- addExpr
  rest <- optionMaybe (try $ do
    spaces
    op <- compOp
    spaces
    e2 <- addExpr
    return (op, e2))
  case rest of
    Nothing -> return e1
    Just (op, e2) -> return $ Op op e1 e2

compOp :: GenParser Char st Binop
compOp = try (string ">=" >> return Ge)
     <|> try (string "<=" >> return Le)
     <|> try (string "==" >> return Eq)
     <|> try (string ">" >> return Gt)
     <|> try (string "<" >> return Lt)

-- Addition/subtraction expression
addExpr :: GenParser Char st Expression
addExpr = chainl1 mulExpr addOp

addOp :: GenParser Char st (Expression -> Expression -> Expression)
addOp = try $ do
  spaces
  op <- (char '+' >> return Plus) <|> (char '-' >> return Minus)
  spaces
  return $ Op op

-- Multiplication/division expression
mulExpr :: GenParser Char st Expression
mulExpr = chainl1 atom mulOp

mulOp :: GenParser Char st (Expression -> Expression -> Expression)
mulOp = try $ do
  spaces
  op <- (char '*' >> return Times) <|> (char '/' >> return Divide)
  spaces
  return $ Op op

-- Atomic expressions
atom :: GenParser Char st Expression
atom = parens
   <|> try skipExpr
   <|> try boolVal
   <|> try intVal
   <|> varExpr

-- Parenthesized expression
parens :: GenParser Char st Expression
parens = do
  char '('
  spaces
  e <- expression
  spaces
  char ')'
  return e

-- Skip expression
skipExpr :: GenParser Char st Expression
skipExpr = do
  string "skip"
  notFollowedBy alphaNum
  return Skip

-- Boolean value
boolVal :: GenParser Char st Expression
boolVal = do
  b <- (string "true" >> return True) <|> (string "false" >> return False)
  notFollowedBy alphaNum
  return $ Val $ BoolVal b

-- Integer value
intVal :: GenParser Char st Expression
intVal = do
  sign <- optionMaybe (char '-')
  digits <- many1 digit
  let n = read digits
  case sign of
    Just _  -> return $ Val $ IntVal (-n)
    Nothing -> return $ Val $ IntVal n

-- Variable expression
varExpr :: GenParser Char st Expression
varExpr = do
  var <- identifier
  return $ Var var

-- Identifier (variable name)
identifier :: GenParser Char st String
identifier = do
  first <- letter
  rest <- many (alphaNum <|> char '_')
  let name = first : rest
  if name `elem` reserved
    then unexpected ("reserved word " ++ name)
    else return name

-- Reserved words
reserved :: [String]
reserved = ["if", "then", "else", "end", "while", "do", "true", "false", "skip", "and", "or", "not"]
