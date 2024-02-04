module LanguageExtension.OverloadedLabelsSpec (spec) where

import           Test.Hspec
import           Util

sub1Spec :: SpecWith ()
sub1Spec = do
  it "sub1" $ do
    1 `shouldBe` 1

----------------------------------------------------------------------------------------------------

sub2Spec :: SpecWith ()
sub2Spec = do
  it "sub2" $ do
    2 `shouldBe` 2

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  sub1Spec
  sub2Spec

main :: IO ()
main = hspec spec
