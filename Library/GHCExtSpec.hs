module Library.GHCExtSpec (spec) where

import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "GHCExtSpec" $ do
    it "coerce" $ do
      1 `shouldBe` 1
