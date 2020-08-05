-- | Description: Module for pretty printing json
-- This module will provide a 'prettyPrint' function that takes an 'Aeson.Value'
-- to a 'Text'. It will format the json with a newline after each curly brace
-- or square brace, a newline after each list item, indention for each nesting,
-- and proper quoting for string literals.
module JL.PrettyPrint where

import Control.Lens (over, view)
import Control.Monad ((>=>))
import Control.Monad.Reader (Reader, asks, local, runReader)
import qualified Data.Aeson as A
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

prettyPrint :: Text -> A.Value -> Text
prettyPrint pfx = flip runReader config . unPrettyPrint . mkPretty
  where config = PPConfig { indentBy = pfx
                          , prefix = ""
                          }

class (Monad m) => Indenter m where
  indent :: Text -> m Text
  nextLevel :: m a -> m a

data PPConfig = PPConfig
  { indentBy :: Text
  , prefix :: Text
  }
  deriving stock (Generic, Show, Eq)

newtype PrettyPrint a = PrettyPrint { unPrettyPrint :: Reader PPConfig a }
  deriving newtype (Functor, Applicative, Monad)

instance Indenter PrettyPrint where
  indent t = PrettyPrint $ (<> t) <$> asks prefix
  nextLevel m = PrettyPrint
    $ local (\r -> over #prefix (view #indentBy r <>) r)
    $ unPrettyPrint m

mkPretty :: (Indenter m) => A.Value -> m Text
mkPretty = \case
  A.Object o -> wrap "{" "}" $ prettyObject o
  A.Array a -> wrap "[" "]" $ prettyArray a
  A.String s -> pure $ "\"" <> s <> "\""
  A.Number n -> pure $ case toBoundedInteger @Int n of
                         Just i -> tshow i
                         Nothing -> tshow n
  A.Bool b -> pure $ T.toLower $ tshow b
  A.Null -> pure "null"
  where
    tshow :: (Show a) => a -> Text
    tshow = T.pack . show

prettyObject :: (Indenter m) => HashMap Text A.Value -> m Text
prettyObject =
  pure . Map.toList
  >=> traverse (sequence . fmap (nextLevel . mkPretty))
  >=> traverse indent . fmap mkKeyValue
  >=> pure . linesWithCommas
  where
    mkKeyValue :: (Text, Text) -> Text
    mkKeyValue (k, v) = "\"" <> k <> "\": " <> v

prettyArray :: (Indenter m) => Vector A.Value -> m Text
prettyArray =
  traverse mkPretty
  >=> traverse indent
  >=> pure . linesWithCommas . V.toList

linesWithCommas :: [Text] -> Text
linesWithCommas = T.intercalate ",\n"

wrap :: (Indenter m) => Text -> Text -> m Text -> m Text
wrap open close body = do
  c <- indent close
  b <- nextLevel body
  pure $ T.intercalate "\n" $ [open, b, c]
