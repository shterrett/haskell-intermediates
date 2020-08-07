module JL.Focus.Parse where

import Text.Parsec (parse, Parsec, many, (<|>), try)
import Text.Parsec.Char (char, noneOf, string, digit)
import Data.Text (Text)
import qualified Data.Text as T

data Focus =
  Key Text
  | Array
  | AnItem Int
  deriving (Show, Eq)

parseFocus :: Text -> Either Text [Focus]
parseFocus t = case parse focusParser "" t of
               Left e -> Left $ T.pack $ show e
               Right fs -> Right fs

focusParser :: Parsec Text () [Focus]
focusParser = many (try keyParser
                    <|> try arrayParser
                    <|> try itemParser
                   )

keyParser :: Parsec Text () Focus
keyParser = Key <$> (char '.' *> textParser)

arrayParser :: Parsec Text () Focus
arrayParser = string "[*]" *> pure Array

itemParser :: Parsec Text () Focus
itemParser = AnItem . read <$> (char '[' *> many digit <* char ']')

textParser :: Parsec Text () Text
textParser = T.pack <$> many (noneOf ".[")
