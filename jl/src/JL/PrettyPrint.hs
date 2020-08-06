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

-- | The "main" function for the 'PrettyPrint' module. It runs the 'PrettyPrint'
-- monad, and delegates the work to 'mkPretty'
prettyPrint :: Text -- ^ The string to be used for each indentation
            -> A.Value -- ^ The json to be pretty printed
            -> Text -- ^ pretty printed json
prettyPrint pfx = flip runReader config . unPrettyPrint . mkPretty
  where config = PPConfig { indentBy = pfx
                          , prefix = ""
                          }

-- | The "language" for pretty printing. Essentially, we are just adding
-- newlines and indentation. This defines the interface for indentation, which
-- requires keeping track of how far in we are indented and for being able to
-- indent yet-another-level
class (Monad m) => Indenter m where
  indent :: Text -> m Text
  -- ^ returns the given string with the current indentation prefixed
  nextLevel :: m a -> m a
  -- ^ adds another level of indentation to the current prefix and executes the
  -- given action
  -- Calls to 'nextLevel' continue to nest, pushing indentation levels onto the
  -- stack, as child 'Value's are pretty-printed.

-- | This contains the state of indentation.
data PPConfig = PPConfig
  { indentBy :: Text -- ^ The indentation that is passed in to `prettyPrint` above
                     -- this is prepended once per indentation level
  , prefix :: Text -- ^ The current indentation prefix.
                   -- Each time the level is increased, using 'nextLevel' above,
                   -- 'indentBy' is appended to 'prefix'.
  }
  deriving stock (Generic, Show, Eq)

-- | The newtype in which we are executing all of the pretty printing. The
-- functions below have only the 'Indenter' class constraint, but in
-- 'prettyPrint', @flip runReader config . unPrettyPrint@ specializes the @m@ to
-- be 'PrettyPrint'
newtype PrettyPrint a = PrettyPrint { unPrettyPrint :: Reader PPConfig a }
  deriving newtype (Functor, Applicative, Monad)

-- | This is where the magic happens. We chose 'Reader' above instead of 'State'
-- to keep track of the current indentation because 'Reader' has a 'local'
-- method that allows its state to be modified to carry out an action, and then
-- the modification is discarded. This is effectively a stack onto which we can
-- push another level of indentation to pretty-print a nested @Value@, and
-- pop it to resume pretty-printing the current @Value@
instance Indenter PrettyPrint where
  indent t = PrettyPrint $ (<> t) <$> asks prefix
  nextLevel m = PrettyPrint
    $ local (\r -> over #prefix (view #indentBy r <>) r)
    $ unPrettyPrint m

-- | The function 'prettyPrint' delegates to. It simply dispatches based on the
-- constructor of 'A.Value'
-- Note that 'A.String' is given an explicit set of quotes so that they show up
-- when the 'Text' is printed.
-- Numbers are also handled in a special way: if the number is an integer, it is
-- printed as an integer; if it's not, it's printed as a float.
-- Javascript only has @double@ so this distinction isn't really a thing, but it
-- also /pretends/ it's a thing. However, we lose the pretense by discarding the
-- original textual representation and converting to 'Scientific'
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

-- | Pretty prints an object by taking it to a @(key, value)@ pair,
-- pretty printing each @value@ with the 'nextLevel' of indentation.
-- Then the @(key, value)@ is converted to @"key": value@
-- Finally, each pair is concatenated with a comma and newline.
prettyObject :: (Indenter m) => HashMap Text A.Value -> m Text
prettyObject =
  pure . Map.toList
  >=> traverse (sequence . fmap (nextLevel . mkPretty))
  >=> traverse indent . fmap mkKeyValue
  >=> pure . linesWithCommas
  where
    mkKeyValue :: (Text, Text) -> Text
    mkKeyValue (k, v) = "\"" <> k <> "\": " <> v

-- | Pretty prints an array by placing each item on a new (indented) line,
-- separated by commas
prettyArray :: (Indenter m) => Vector A.Value -> m Text
prettyArray =
  traverse mkPretty
  >=> traverse indent
  >=> pure . linesWithCommas . V.toList

-- | Helper function that terminates each 'Text' in @[Text]@ with a comma and a newline.
-- Except the last one.
linesWithCommas :: [Text] -> Text
linesWithCommas = T.intercalate ",\n"

-- | Helper function to wrap a pretty printed json object with delimiters
-- Either @{...}@ for objects or @[...]@ for arrays.
wrap :: (Indenter m)
     => Text -- ^ Opening delimiter
     -> Text -- ^ Closing delimiter
     -> m Text -- ^ innards to be pretty printed
     -> m Text -- ^ value, surrounded by delimiters (each on a new line)
               -- with all inner bits indented one level further
wrap open close body = do
  c <- indent close
  b <- nextLevel body
  pure $ T.intercalate "\n" $ [open, b, c]
