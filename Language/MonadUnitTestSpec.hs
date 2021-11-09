module Language.MonadUnitTestSpec (spec) where

import Test.Hspec

-- To test function
charNumInFile :: FilePath -> IO Int
charNumInFile f = readFile f >>= \content -> return (length content)

mockFileSystemSpec :: SpecWith ()
mockFileSystemSpec = do
  it "mock filesystem" $ do
    1 `shouldBe` 1

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  mockFileSystemSpec