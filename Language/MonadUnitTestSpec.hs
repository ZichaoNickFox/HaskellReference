module Language.MonadUnitTestSpec (spec) where

import Test.Hspec

mockFileSystemSpec :: SpecWith ()
mockFileSystemSpec = do
  it "mock filesystem" $ do
    1 `shouldBe` 1

spec :: SpecWith ()
spec = do
  describe "Unit Test Spec" $ do
    mockFileSystemSpec