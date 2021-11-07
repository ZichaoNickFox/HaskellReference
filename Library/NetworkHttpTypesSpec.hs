{-# LANGUAGE OverloadedStrings #-}
module Library.NetworkHttpTypesSpec (spec) where

import Network.HTTP.Types.Status
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "http types" $ do
    it "status200" $ do
      statusCode status200 `shouldBe` 200
      statusMessage status200 `shouldBe` "OK"
      toEnum 200 `shouldBe` status200
      fromEnum status200 `shouldBe` 200