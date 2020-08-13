-- | Description: parses the focus string into @[Focus}@ that can be processed
-- by @JL.Focus@
module JL.Focus.Parse where

import Text.Parsec (parse, Parsec, many1, (<|>), try)
import Text.Parsec.Char (char, noneOf, string, digit)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

-- | the result of parsing each segment of a string
data Focus =
  Key Text -- ^ from a key ame
  | Array -- ^ from [*]
  | AnItem Int -- ^ from [key name]
  deriving (Show, Eq)

-- | inverse of parsing, this returns the '[Focus]' to a focus string
-- There is a quickcheck test to ensure that this and parsing are inverses
formatFocus :: Focus -> Text
formatFocus (Key k) = "." <> k
formatFocus Array = "[*]"
formatFocus (AnItem i) = "[" <> T.pack (show i) <> "]"

-- | the main function, it parses a focus string into '[Focus]'
-- Given a string like firstKey.secondKey[*].thirdKey.[fourthKey]
-- it returns '[Key "firstKey", Key "secondKey", Array, Key "thirdKey", AnItem "fourthKey"]'
parseFocus :: Text -> Either Text (NonEmpty Focus)
parseFocus t = case parse focusParser "" t of
               Left e -> Left $ T.pack $ show e
               Right fs -> maybe (Left "Parsed empty list of foci") Right
                            $ NE.nonEmpty fs

-- The parser for the overall string. Returns zero or more of either key, array,
-- or items
focusParser :: Parsec Text () [Focus]
focusParser = many1 (try keyParser
                      <|> try arrayParser
                      <|> try itemParser
                    )

-- | parses '".name"' to 'Key "name"'
keyParser :: Parsec Text () Focus
keyParser = Key <$> (char '.' *> textParser)

-- | parses '"[*]"' to 'Array'
arrayParser :: Parsec Text () Focus
arrayParser = string "[*]" *> pure Array

-- | parses '"[name]"' to 'AnItem "name"'
itemParser :: Parsec Text () Focus
itemParser = AnItem . read <$> (char '[' *> many1 digit <* char ']')

-- | basic definition of allowed text. This is probably too simple, but is
-- generally ok.
textParser :: Parsec Text () Text
textParser = T.pack <$> many1 (noneOf ".[")
