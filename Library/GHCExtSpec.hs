module Library.GHCExtSpec (spec) where

import           Test.Hspec

spec :: SpecWith ()
spec = do
  it "coerce" $ do
    1 `shouldBe` 1

main :: IO ()
main = hspec spec
