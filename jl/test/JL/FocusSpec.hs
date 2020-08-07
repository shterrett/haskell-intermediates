module JL.FocusSpec where

import JL.Focus
import JL.Focus.Parse (Focus(..))

import Control.Lens (preview, toListOf, Fold)
import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "focusing on json structure" $ do
  let json =
        [aesonQQ|
          {
            "kind": "youtube#searchListResponse",
            "etag": "\"m2yskBQFythfE4irbTIeOgYYfBU/PaiEDiVxOyCWelLPuuwa9LKz3Gk\"",
            "nextPageToken": "CAUQAA",
            "regionCode": "KE",
            "pageInfo": {
              "totalResults": 4249,
              "resultsPerPage": 5
            },
            "items": [
              {
                "kind": "youtube#searchResult",
                "etag": "\"m2yskBQFythfE4irbTIeOgYYfBU/QpOIr3QKlV5EUlzfFcVvDiJT0hw\"",
                "id": {
                  "kind": "youtube#channel",
                  "channelId": "UCJowOS1R0FnhipXVqEnYU1A"
                }
              },
              {
                "kind": "youtube#searchResult",
                "etag": "\"m2yskBQFythfE4irbTIeOgYYfBU/AWutzVOt_5p1iLVifyBdfoSTf9E\"",
                "id": {
                  "kind": "youtube#video",
                  "videoId": "Eqa2nAAhHN0"
                }
              },
              {
                "kind": "youtube#searchResult",
                "etag": "\"m2yskBQFythfE4irbTIeOgYYfBU/2dIR9BTfr7QphpBuY3hPU-h5u-4\"",
                "id": {
                  "kind": "youtube#video",
                  "videoId": "IirngItQuVs"
                }
              }
            ]
          }
        |]
  it "selects a top-level key" $
    focusOn (NE.fromList [Key "kind"]) json `shouldBe`
      Right ([Key "kind"],  getKey (A.key "kind") json)
  it "selects the first item in an array" $
    focusOn (NE.fromList [Key "items", AnItem 0]) json `shouldBe`
      Right ([Key "items", AnItem 0], getKey (A.key "items" . A.nth 0) json)
  it "returns the outer object when the focus path fails" $
    focusOn (NE.fromList [Key "pageInfo", Key "resultCount"]) json `shouldBe`
      Left ([Key "pageInfo"], getKey (A.key "pageInfo") json)
  it "gets all items in an array" $
    focusOn (NE.fromList [Key "items", Array]) json `shouldBe`
      Right ([Key "items", Array], getArray (A.key "items" . A.values) json)
  it "gets a key from each object inside an array" $
    focusOn (NE.fromList [Key "items", Array, Key "kind"]) json `shouldBe`
      Right ([Key "items", Array, Key "kind"], getArray (A.key "items" . A.values . A.key "kind") json)
  it "returns the out object when no array is present" $ do
    focusOn (NE.fromList [Key "pageInfo", AnItem 0]) json `shouldBe`
      Left ([Key "pageInfo"], getKey (A.key "pageInfo") json)
    focusOn (NE.fromList [Key "pageInfo", Array]) json `shouldBe`
      Left ([Key "pageInfo"], getKey (A.key "pageInfo") json)

getKey :: Fold A.Value A.Value -> A.Value -> A.Value
getKey l = fromJust . preview l

getArray :: Fold A.Value A.Value -> A.Value -> A.Value
getArray l = A.Array . V.fromList . toListOf l
