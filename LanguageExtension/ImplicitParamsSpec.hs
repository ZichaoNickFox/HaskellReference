{-# LANGUAGE ImplicitParams #-}

module LanguageExtension.ImplicitParamsSpec (spec) where

import qualified Data.List as L
import Test.Hspec
import Util

-- https://www.haskell.org/hugs/pages/users_guide/implicit-parameters.html
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = L.sortBy

sort :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort = sortBy ?cmp

sortSpec :: SpecWith ()
sortSpec = do
  it "sortSpec" $ do
    let ?cmp = compare
    sort ([2, 1, 4, 3] :: [Int]) `shouldBe` [1, 2, 3, 4]

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  sortSpec