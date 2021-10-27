module Template (spec) where

import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "template" $ do
    it "sub" $ do
      1 `shouldBe` 1