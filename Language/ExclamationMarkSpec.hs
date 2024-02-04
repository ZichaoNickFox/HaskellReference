module Language.ExclamationMarkSpec (spec) where

import           Control.Exception
import           Test.Hspec
import           Util

data Foo = Foo {
  first  :: Int,
  second :: !Int
}

undefinedSpec :: SpecWith ()
undefinedSpec = do
  it "undefined" $ do
    second (Foo undefined 1) `shouldBe` 1
    evaluate (first (Foo 1 undefined)) `shouldThrow` anyException

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  undefinedSpec

main :: IO ()
main = hspec spec
