module JL.Focus.Parse where

import Text.Parsec (parse, Parsec, many1, (<|>), try)
import Text.Parsec.Char (char, noneOf, string, digit)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

data Focus =
  Key Text
  | Array
  | AnItem Int
  deriving (Show, Eq)

formatFocus :: Focus -> Text
formatFocus (Key k) = "." <> k
formatFocus Array = "[*]"
formatFocus (AnItem i) = "[" <> T.pack (show i) <> "]"

parseFocus :: Text -> Either Text (NonEmpty Focus)
parseFocus t = case parse focusParser "" t of
               Left e -> Left $ T.pack $ show e
               Right fs -> maybe (Left "Parsed empty list of foci") Right
                            $ NE.nonEmpty fs

focusParser :: Parsec Text () [Focus]
focusParser = many1 (try keyParser
                      <|> try arrayParser
                      <|> try itemParser
                    )

keyParser :: Parsec Text () Focus
keyParser = Key <$> (char '.' *> textParser)

arrayParser :: Parsec Text () Focus
arrayParser = string "[*]" *> pure Array

itemParser :: Parsec Text () Focus
itemParser = AnItem . read <$> (char '[' *> many1 digit <* char ']')

textParser :: Parsec Text () Text
textParser = T.pack <$> many1 (noneOf ".[")
