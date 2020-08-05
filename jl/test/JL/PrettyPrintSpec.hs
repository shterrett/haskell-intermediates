module JL.PrettyPrintSpec where

import JL.Arbitrary ()
import JL.PrettyPrint

import qualified Data.Aeson as A
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as T
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "pretty printing properties" $ do
    it "parses back to the original json" $ property $
      \v -> (A.eitherDecode . BS.fromStrict . T.encodeUtf8 . prettyPrint "  " $ v) == Right v
  describe "pretty printing json examples" $ do
    it "pretty prints null" $ do
      let json = [aesonQQ| null |]
      prettyPrint "  " json `shouldBe` "null"
    it "pretty prints a string" $ do
      let json = [aesonQQ| "string" |]
      prettyPrint "  " json `shouldBe` "\"string\""
    it "pretty prints a boolean" $ do
      let json = [aesonQQ| true |]
      prettyPrint "  " json `shouldBe` "true"
    it "pretty prints a number" $ do
      let int = [aesonQQ| 5 |]
      prettyPrint "  " int `shouldBe` "5"
      let float = [aesonQQ| 3.14 |]
      prettyPrint "  " float `shouldBe` "3.14"
      let almostInt = [aesonQQ| 5.0 |]
      prettyPrint "  " almostInt `shouldBe` "5"
    it "pretty prints an array" $ do
      let json = [aesonQQ| ["x", "y", "z"] |]
          expected = "[\n"
                  <> "  \"x\",\n"
                  <> "  \"y\",\n"
                  <> "  \"z\"\n"
                  <> "]"
      prettyPrint "  " json `shouldBe` expected
    it "pretty prints an object" $ do
      let json = [aesonQQ| { "x": true, "y": 3.14, "z": "huh?" } |]
          expected = "{\n  \"z\": \"huh?\",\n  \"x\": true,\n  \"y\": 3.14\n}"
      prettyPrint "  " json `shouldBe` expected
    it "pretty prints a nested, real world example" $ do
      let json = [aesonQQ| {
                  "glossary": { "title": "example glossary", "GlossDiv": { "title": "S",
                    "GlossList": { "GlossEntry": { "ID": "SGML", "SortAs": "SGML",
                        "GlossTerm": "Standard Generalized Markup Language", "Acronym": "SGML",
                        "Abbrev": "ISO 8879:1986",
                        "GlossDef": { "para": "A meta-markup language, used to create markup languages such as DocBook.",
                        "GlossSeeAlso": ["GML", "XML"]
                                  }, "GlossSee": "markup" } } } } }
                 |]
          expected = "{\n  \"glossary\": {\n      \"GlossDiv\": {\n          \"title\": \"S\",\n          \"GlossList\": {\n              \"GlossEntry\": {\n                  \"GlossSee\": \"markup\",\n                  \"Acronym\": \"SGML\",\n                  \"Abbrev\": \"ISO 8879:1986\",\n                  \"GlossTerm\": \"Standard Generalized Markup Language\",\n                  \"ID\": \"SGML\",\n                  \"SortAs\": \"SGML\",\n                  \"GlossDef\": {\n                      \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",\n                      \"GlossSeeAlso\": [\n                          \"GML\",\n                          \"XML\"\n                        ]\n                    }\n                }\n            }\n        },\n      \"title\": \"example glossary\"\n    }\n}"
      prettyPrint "  " json `shouldBe` expected

