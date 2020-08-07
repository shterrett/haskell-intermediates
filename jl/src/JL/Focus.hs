module JL.Focus where

import JL.Focus.Parse (Focus(..))

import Control.Lens ((^?), Fold, (^..))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A
import qualified Data.Vector as V

data Accessor =
  Preview
  | ToListOf
  deriving (Show, Eq, Ord)

data Level = Level
  { levels :: [Focus]
  , optic :: Fold A.Value A.Value
  , accessor :: Accessor
  }

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

doFocus :: A.Value -> Level -> Maybe ([Focus], A.Value)
doFocus json Level{..} =
  case accessor of
    Preview -> (levels,) <$> json ^? optic
    ToListOf -> case json ^.. optic of
                  [] -> Nothing
                  as -> Just (levels, A.Array $ V.fromList as)

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
