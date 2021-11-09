module Template (spec) where

import Test.Hspec

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
  describe "template" $ do
    sub1Spec
    sub2Spec