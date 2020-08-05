{-# OPTIONS_GHC -fno-warn-orphans #-}

module JL.Arbitrary where

import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.QuickCheck (
  Arbitrary(..), arbitraryPrintableChar, Gen, oneof, sized, choose
  , vectorOf
  )

instance Arbitrary A.Value where
  arbitrary = sized genValue

genValue :: Int -> Gen A.Value
genValue 0 = oneof [ A.String <$> genString
                    , A.Bool <$> arbitrary
                    , pure A.Null
                   ]
genValue n = oneof [ A.Object <$> genObject n
                   , A.Array <$> genArray n
                   , A.String <$> genString
                   , A.Bool <$> arbitrary
                   , pure A.Null
                   ]

genObject :: Int -> Gen (HashMap Text A.Value)
genObject n = do
  l <- choose (0, n)
  ks <- nub <$> vectorOf l genString
  vs <- vectorOf l (genValue $ n `div` 2)
  pure $ Map.fromList $ zip ks vs

genArray :: Int -> Gen (Vector A.Value)
genArray n = do
  l <- choose (0, n)
  V.fromList <$> vectorOf l (genValue $ n `div` 2)

genString :: Gen Text
genString = T.pack . filter (not . flip elem ['\\', '"']) <$> vectorOf 2 arbitraryPrintableChar
