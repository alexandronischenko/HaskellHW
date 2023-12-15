module ParserCSV where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

import Text.Parsec
import Text.Parsec.String

parseDataRow :: Parser DataRow
parseDataRow = DataRow
  <$> (many1 digit <* char ',')
  <*> (quotedString <* char ',')
  <*> (many1 digit <* char ',')
  <*> (quotedString <* char ',')
  <*> (quotedString <* char ',')
  <*> (read <$> many1 digit)
  where
    quotedString = char '"' *> many (noneOf "\"") <* char '"'
