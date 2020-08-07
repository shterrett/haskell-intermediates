module JL.Focus.ParseSpec where

import JL.Focus.Parse
import JL.Arbitrary ()

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = describe "parsing focus paths" $ do
  it "parses a single key" $ do
    parseFocus ".key" `shouldBe` Right (NE.fromList [Key "key"])
  it "parses an 'all array' access" $ do
    parseFocus "[*]" `shouldBe` Right (NE.fromList [Array])
  it "parses a single-element array access" $ do
    parseFocus "[3]" `shouldBe` Right (NE.fromList [AnItem 3])
  it "parses chained keys" $ do
    parseFocus ".keyOne.keyTwo.keyThree" `shouldBe`
      Right (NE.fromList [Key "keyOne", Key "keyTwo", Key "keyThree"])
  it "mixes keys and array accesses" $ do
    parseFocus ".keyOne[*].keyTwo.keyThree[3].keyFour" `shouldBe`
      Right (NE.fromList [Key "keyOne", Array, Key "keyTwo", Key "keyThree", AnItem 3, Key "keyFour"])
  describe "parse and format properties" $ do
    it "inverse operations" $ property $
      \(fs :: NonEmpty Focus) -> parseFocus (foldMap formatFocus fs) == Right fs
