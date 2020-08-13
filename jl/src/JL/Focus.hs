-- | Description: building lens into the json
--
-- This module handles the list of focuses returned from parsing the focus
-- string
-- The list is transformed into a list of accessors, and then these are composed
-- together. Each @init@ of this list is run independently, and the longest one
-- to succeed is reported. If the full list succeeds, then the focus was a
-- success. Otherwise, it was a failure, and the longest successful focus is
-- returned.

module JL.Focus where

import JL.Focus.Parse (Focus(..))

import Control.Lens ((^?), Fold, (^..))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A
import qualified Data.Vector as V

-- | Uses the language of lens to represent each focus. A @Key@ or @AnItem@ will
-- become a @Prism@, and thus will be called with @preview@. An @Array@ will
-- become a @Fold@ and thus be called with @toListOf@
data Accessor =
  Preview
  | ToListOf
  deriving (Show, Eq, Ord)

-- | Each of the @init@s of the list (built by a scan) is a "level" of focusing.
-- So, for a list of focuses '[Key "a", AnItem "b", Key "c"]', the first level
-- will have only '[Key "a"]', the second '[Key "a", AnItem "b"]', and the third
-- will have '[Key "a", AnItem "b", Key "c"]'. Each level can then be applied to
-- the json and succeed or fail independently.
data Level = Level
  { levels :: [Focus] -- ^ the list of focuses for error reporting
  , optic :: Fold A.Value A.Value -- ^ the actual composed optic that will be applied to the json
  , accessor :: Accessor -- ^ the way in which the optic will be called
  }

-- | the main function in this module. Given a list of focuses and a json value,
-- attempt to focus on the target. Returns @Right@ if the entire focus string
-- succeeds; otherwise, returns @Left@ with the longest succeeding sub-path.
focusOn :: NonEmpty Focus -> A.Value -> Either ([Focus], A.Value) ([Focus], A.Value)
focusOn fs json =
  let
    rs = mapMaybe (doFocus json) $ buildOptics fs
  in
    case NE.nonEmpty rs of
      Just neRs ->
        if length fs == length neRs
          then Right $ NE.last neRs
          else Left $ NE.last neRs
      Nothing -> Left ([], json)

-- | The actual call of the optic. Essentially, it is 'preview optic', but there
-- is a bit of special logic to convert empty results from @toListOf@ to
-- @Nothing@
doFocus :: A.Value -> Level -> Maybe ([Focus], A.Value)
doFocus json Level{..} =
  case accessor of
    Preview -> (levels,) <$> json ^? optic
    ToListOf -> case json ^.. optic of
                  [] -> Nothing
                  as -> Just (levels, A.Array $ V.fromList as)

-- | the @scanl@ builds a sequence of levels such that each one has one more
-- focus than the last.
buildOptics :: NonEmpty Focus -> [Level]
buildOptics (f :| fs) =
  scanl buildOptic (buildOptic emptyLevel f) fs
  where emptyLevel = Level { levels = []
                           , optic = id
                           , accessor = Preview
                           }

buildOptic :: Level -> Focus -> Level
buildOptic l@Level{..} f =
  l { levels = levels <> [f]
    , optic = optic . toOptic f
    , accessor = max accessor  $ toAccessor f
    }

toOptic :: Focus -> Fold A.Value A.Value
toOptic (Key k) = A.key k
toOptic (AnItem i) = A.nth i
toOptic Array = A.values

toAccessor :: Focus -> Accessor
toAccessor (Key _) = Preview
toAccessor (AnItem _) = Preview
toAccessor Array = ToListOf
