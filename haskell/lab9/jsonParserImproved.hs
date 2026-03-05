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
jsonFile = spaces *> jsonElem <* spaces <* eof

jsonElem :: GenParser Char st JValue
jsonElem = spaces *> jsonElem' <* spaces

jsonElem' = try jsonObj
        <|> try jsonArr
        <|> try jsonString
        <|> try jsonNumber
        <|> try jsonBool
        <|> jsonNull
        <?> "json element"

jsonString :: GenParser Char st JValue
jsonString = try jsonStringDQ <|> jsonStringSQ

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
jsonNumber = many1 digit >>= return . JNumber . read

jsonBool = (string "true" >> return (JBool True))
       <|> (string "false" >> return (JBool False))

jsonNull = string "null" >> return JNull

jsonArr = do
  char '['
  arr <- jsonElem `sepBy` (char ',')
  char ']'
  return $ JArray arr

jsonObj = char '{' *> (jsonPair `sepBy` char ',') <* char '}' >>= return . JObject

jsonPair :: GenParser Char st (String, JValue)
jsonPair = spaces *> jsonKey <* spaces <* char ':' <* spaces >>= \k -> jsonElem <* spaces >>= \v -> return (k, v)

jsonKey :: GenParser Char st String
jsonKey = (char '"' *> many (noneOf "\"") <* char '"')
      <|> (char '\'' *> many (noneOf "'") <* char '\'') 
      <|> many1 (letter <|> digit <|> char '_')



parseJSON :: String -> Either ParseError JValue
parseJSON input = parse jsonFile "(unknown)" input

main = do
  args <- getArgs
  p <- parseFromFile jsonFile (head args)
  case p of
    Left err  -> print err
    Right json -> print json

