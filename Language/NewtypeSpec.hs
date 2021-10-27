module NewtypeSpec (spec) where

import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "newtype spec" $ do
    it "newtype" $ do
      1 `shouldBe` 1