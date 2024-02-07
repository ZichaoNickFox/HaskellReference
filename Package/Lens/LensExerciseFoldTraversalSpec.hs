{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Package.Lens.LensExerciseFoldTraversalSpec where

import Data.Text
import           Control.Lens
import Control.Lens.TH
import Test.Hspec
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.QQ
import Data.Aeson.Lens
import Util (shouldBeWhat)
import qualified Data.Vector as Vector

-- https://williamyaoh.com/posts/2019-04-25-lens-exercises.html

users = [aesonQQ|
  {
    "users": [
      {
        "name": "qiao.yifan",
        "email": "qyifan@xingxin.com",
        "metadata": {
          "num_logins": 5
        }
      },
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 27,
          "associated_ips": [
            "52.49.1.233",
            "52.49.1.234"
          ]
        }
      },
      {
        "name": "su.mucheng",
        "email": "smucheng@xingxin.com",
        "metadata": {
          "associated_ips": [
            "51.2.244.193"
          ]
        }
      }
    ]
  }
|]

foldTraversalISpec :: SpecWith ()
foldTraversalISpec = do
  it "^.. values" $ users ^.. key "users".values.key "email"._String `shouldBe` ["qyifan@xingxin.com", "smucheng@xingxin.com"]
  it "^.. values" $ users ^.. key "users"._Array.traversed.key "email"._String  `shouldBe` ["qyifan@xingxin.com", "smucheng@xingxin.com"]
  it "^.. values" $ users ^.. key "users"._Array.folded.key "email"._String  `shouldBe` ["qyifan@xingxin.com", "smucheng@xingxin.com"]
  it "^. values" $ users ^. key "users".values.key "name"._String `shouldBe` "qiao.yifanye.xiusu.mucheng"
  it "values" $ (users & key "users".values.key "name"._String %~ toUpper) `shouldBe`
    Object (KeyMap.fromList [("users",Array (Vector.fromList [
      Object (KeyMap.fromList [("email",String "qyifan@xingxin.com"),("metadata",Object (KeyMap.fromList [("num_logins",Number 5.0)])),("name",String "QIAO.YIFAN")]),
      Object (KeyMap.fromList [("metadata",Object (KeyMap.fromList [("associated_ips",Array (Vector.fromList [String "52.49.1.233",String "52.49.1.234"])),("num_logins",Number 27.0)])),("name",String "YE.XIU")]),
      Object (KeyMap.fromList [("email",String "smucheng@xingxin.com"),("metadata",Object (KeyMap.fromList [("associated_ips",Array (Vector.fromList [String "51.2.244.193"]))])),("name",String "SU.MUCHENG")])]))])
  it "_Array" $ (users ^..key "users".values.key "metadata".key "associated_ips"._Array) `shouldBe` [Vector.fromList [String "52.49.1.233", String "52.49.1.234"], Vector.fromList [String "51.2.244.193"]]
  it "values" $ (users ^..key "users".values.key "metadata".key "associated_ips".values._String) `shouldBe` ["52.49.1.233", "52.49.1.234", "51.2.244.193"]

lensExerciseFoldTraversalSpec :: SpecWith ()
lensExerciseFoldTraversalSpec = do
  describe "foldTraversalISpec" foldTraversalISpec

main :: IO ()
main = hspec lensExerciseFoldTraversalSpec
