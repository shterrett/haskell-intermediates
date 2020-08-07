module JL.Focus.ParseSpec where

import JL.Focus.Parse

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "parsing focus paths" $ do
  it "parses a single key" $ do
    parseFocus ".key" `shouldBe` Right [Key "key"]
  it "parses an 'all array' access" $ do
    parseFocus "[*]" `shouldBe` Right [Array]
  it "parses a single-element array access" $ do
    parseFocus "[3]" `shouldBe` Right [AnItem 3]
  it "parses chained keys" $ do
    parseFocus ".keyOne.keyTwo.keyThree" `shouldBe`
      Right [Key "keyOne", Key "keyTwo", Key "keyThree"]
  it "mixes keys and array accesses" $ do
    parseFocus ".keyOne[*].keyTwo.keyThree[3].keyFour" `shouldBe`
      Right [Key "keyOne", Array, Key "keyTwo", Key "keyThree", AnItem 3, Key "keyFour"]

