{-
  Module: Parser
  Description: Parsec-based parser for the language.
  Design Decisions:
    - Uses chainl1 for operator precedence.
    - Implements a recursive descent approach for expressions.
    - Handles whitespace and comments gracefully.
-}

module Parser (parseProgram, parseFile) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import AST

-- Language Definition for Tokenizer
languageDef :: LanguageDef st
languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = ["fun", "match", "with", "if", "then", "else", "true", "false"]
  , Token.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "&&", "||", ":", ";", "=", "->", "|"]
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
brackets   = Token.brackets   lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
commaSep   = Token.commaSep   lexer
semi       = Token.semi       lexer

-- Top-level parser
parseProgram :: String -> Either ParseError Program
parseProgram = parse programParser ""

parseFile :: String -> IO (Either ParseError Program)
parseFile filename = do
  content <- readFile filename
  return $ parseProgram content

programParser :: Parser Program
programParser = do
  whiteSpace
  defs <- many (try functionDef)
  expr <- expression
  eof
  return $ Program defs expr

functionDef :: Parser Definition
functionDef = do
  reserved "fun"
  name <- identifier
  params <- parens (commaSep identifier)
  reservedOp "="
  body <- expression
  return $ FunDef name params body

expression :: Parser Expr
expression = sequenceExpr

sequenceExpr :: Parser Expr
sequenceExpr = do
  exprs <- sepBy1 assignmentExpr (reservedOp ";")
  return $ foldr1 SeqExpr exprs

assignmentExpr :: Parser Expr
assignmentExpr = try (do
  var <- identifier
  reservedOp "="
  e <- assignmentExpr
  return $ AssignExpr var e)
  <|> matchExpr

matchExpr :: Parser Expr
matchExpr = (do
  reserved "match"
  target <- expression
  reserved "with"
  cases <- many1 matchCase
  return $ MatchExpr target cases)
  <|> ifExpr

matchCase :: Parser (Pattern, Expr)
matchCase = do
  reservedOp "|"
  pat <- pattern
  reservedOp "->"
  expr <- expression
  return (pat, expr)

pattern :: Parser Pattern
pattern = try (do
  h <- identifier
  reservedOp ":"
  t <- identifier
  return $ PCons h t)
  <|> try (reservedOp "[]" >> return PListNil)
  <|> (char '_' >> return PWildcard)
  <|> (PInt . fromInteger <$> integer)
  <|> (reserved "true" >> return (PBool True))
  <|> (reserved "false" >> return (PBool False))
  <|> (PVar <$> identifier)

ifExpr :: Parser Expr
ifExpr = (do
  reserved "if"
  cond <- expression
  reserved "then"
  thenPart <- expression
  reserved "else"
  elsePart <- expression
  return $ IfExpr cond thenPart elsePart)
  <|> opExpr

opExpr :: Parser Expr
opExpr = buildExpressionParser table term <?> "expression"

table = [ [Infix (reservedOp ":" >> return ConsExpr) AssocRight]
        , [Infix (reservedOp "*" >> return (OpExpr Times)) AssocLeft,
           Infix (reservedOp "/" >> return (OpExpr Divide)) AssocLeft]
        , [Infix (reservedOp "+" >> return (OpExpr Plus))  AssocLeft,
           Infix (reservedOp "-" >> return (OpExpr Minus)) AssocLeft]
        , [Infix (reservedOp "==" >> return (OpExpr Eq)) AssocNone,
           Infix (reservedOp "<"  >> return (OpExpr Lt)) AssocNone,
           Infix (reservedOp ">"  >> return (OpExpr Gt)) AssocNone,
           Infix (reservedOp "<=" >> return (OpExpr Le)) AssocNone,
           Infix (reservedOp ">=" >> return (OpExpr Ge)) AssocNone]
        , [Infix (reservedOp "&&" >> return (OpExpr And)) AssocLeft]
        , [Infix (reservedOp "||" >> return (OpExpr Or))  AssocLeft]
        ]

term = parens expression
     <|> try callExpr
     <|> listExpr
     <|> (ValExpr . IntVal . fromInteger <$> integer)
     <|> (reserved "true" >> return (ValExpr (BoolVal True)))
     <|> (reserved "false" >> return (ValExpr (BoolVal False)))
     <|> (VarExpr <$> identifier)
     <?> "simple expression"

callExpr :: Parser Expr
callExpr = do
  name <- identifier
  args <- parens (commaSep expression)
  return $ CallExpr name args

listExpr :: Parser Expr
listExpr = ListExpr <$> brackets (commaSep expression)
