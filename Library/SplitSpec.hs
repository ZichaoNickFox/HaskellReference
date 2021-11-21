module Library.SplitSpec (spec) where

import Data.List.Split
import Test.Hspec
import Util

splitSpec :: SpecWith ()
splitSpec = do
  it "split spec" $ do
    splitWhen (=='.') "www.zichaonickfox.com" `shouldBe` ["www", "zichaonickfox", "com"]

spec :: SpecWith ()
spec = do
  splitSpec