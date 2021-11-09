module Language.DataSpec (spec) where

import Prelude
import Test.Hspec

data List a = Nil | a :. List a deriving (Show, Eq)

listSpec :: SpecWith ()
listSpec = do
  it "listSpec" $ do
    'a' :. Nil `shouldBe` 'a' :. Nil

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  describe "DataSpec" $ do
    listSpec