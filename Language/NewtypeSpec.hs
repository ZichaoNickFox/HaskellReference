module Language.NewtypeSpec (spec) where

import Test.Hspec

newtype Age = Age Int deriving (Show, Eq)
newtype Param a b = Param (Either a b) deriving (Show, Eq)
newtype Record = Record { getInt :: Int } deriving (Show, Eq)

spec :: SpecWith ()
spec = do
  describe "newtype spec" $ do
    it "newtype" $ do
      Age 1 `shouldBe` Age 1
      (Param (Left 1 :: Either Int String)) `shouldBe` (Param (Left 1 :: Either Int String))
      Record 2021 `shouldBe` Record 2021