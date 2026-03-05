import Text.ParserCombinators.Parsec
import System.Environment

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
  deriving (Eq, Ord, Show)


jsonFile :: GenParser Char st JValue
jsonFile = do
  result <- jsonElem
  eof
  return result

jsonElem :: GenParser Char st JValue
jsonElem = do
  spaces
  result <- jsonElem'
  spaces
  return result

jsonElem' = jsonObj
        <|> jsonArr
        <|> jsonString
        <|> jsonNumber
        <|> jsonBool
        <|> jsonNull
        <?> "json element"

jsonString :: GenParser Char st JValue
jsonString = jsonStringDQ <|> jsonStringSQ

jsonStringDQ = do
  char '"'
  s <- many $ noneOf "\"" -- crude.  does not allow double quotes within strings
  char '"'
  return $ JString s

jsonStringSQ = do
  char '\''
  s <- many $ noneOf "'" -- crude, same as above
  char '\''
  return $ JString s

jsonNumber :: GenParser Char st JValue
jsonNumber = do
  num <- many1 digit
  return $ JNumber (read num)

jsonBool = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> JBool True
    "false" -> JBool False

jsonNull = do
  string "null"
  return JNull

jsonArr = do
  char '['
  arr <- jsonElem `sepBy` (char ',')
  char ']'
  return $ JArray arr

jsonObj = do
  char '{'
  pairs <- jsonPair `sepBy` (char ',')
  char '}'
  return $ JObject pairs

jsonPair :: GenParser Char st (String, JValue)
jsonPair = do
  spaces
  key <- jsonKey
  spaces
  char ':'
  spaces
  value <- jsonElem
  spaces
  return (key, value)

jsonKey :: GenParser Char st String
jsonKey = do
  (char '"' >> many (noneOf "\"") >>= \s -> char '"' >> return s)
  <|> (char '\'' >> many (noneOf "'") >>= \s -> char '\'' >> return s)
  <|> many1 (letter <|> digit <|> char '_')



parseJSON :: String -> Either ParseError JValue
parseJSON input = parse jsonFile "(unknown)" input

main = do
  args <- getArgs
  p <- parseFromFile jsonFile (head args)
  case p of
    Left err  -> print err
    Right json -> print json

