

module Library.SpecTypeRepMap (specs) where

import Data.TMap
import Test.Hspec
import Util

tm :: TMap
tm = insert True $ one (42::Int)

specs :: SpecWith ()
specs = do
  describe "TypeRepMap" $ do
    it "show" $ do
      (show tm) `shouldBe` "TypeRepMap [Bool, Int]"

    it "insert" $ do
      size tm `shouldBe` 2

    -- it "lookup" $ do
    --   (lookup tm :: Maybe Int) `shouldBe` 42
