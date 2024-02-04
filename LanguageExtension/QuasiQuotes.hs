{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson.QQ
import Data.Aeson.Lens
import           Test.Hspec
import           Control.Lens

user1 = [aesonQQ|
  {
    "name": "qiao.yifan",
    "email": "qyifan@xingxin.com"
  }
|]

spec :: SpecWith ()
spec = do
  it "QuasiQuotes" $ user1 ^. key "name"._String `shouldBe` "qiao.yifan"

main :: IO ()
main = hspec spec
