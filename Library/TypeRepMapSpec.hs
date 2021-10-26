{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Library.TypeRepMapSpec (spec) where

import Data.TMap
import Prelude hiding (lookup)
import Test.Hspec
import Util

tm :: TMap
tm = insert True $ one (42::Int)

spec :: SpecWith ()
spec = do
  describe "TypeRepMap" $ do
    it "show" $ do
      (show tm) `shouldBe` "TypeRepMap [Bool, Int]"

    it "insert" $ do
      size tm `shouldBe` 2

    it "lookup" $ do
      (lookup tm :: Maybe Int) `shouldBe` (Just 42)
      (lookup tm :: Maybe Bool) `shouldBe` (Just True)
      (lookup tm :: Maybe String) `shouldBe` Nothing
    
    it "member" $ do
      (member @Int tm) `shouldBe` True
      (member @Bool tm) `shouldBe` True
      (member @String tm) `shouldBe` False
