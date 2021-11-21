
{-# LANGUAGE RankNTypes #-}

module LanguageExtension.RankNTypesSpec (spec) where

import Test.Hspec
import Util

intDoublePolymorphic :: (forall n. Num n => n -> n) -> (Int, Double)
intDoublePolymorphic f = (f 1, f 1.0)

-- The follow 2 expresses to constrait n is wrong
-- https://stackoverflow.com/questions/33446759/understanding-haskells-rankntypes

-- intDoublePolymorphic :: Num n => (n -> n) -> (Int, Double)
-- intDoublePolymorphic  f = (f 1, f 1.0)

-- intDoublePolymorphic :: forall n. Num n => (n -> n) -> (Int, Double)
-- intDoublePolymorphic  f = (f 1, f 1.0)

intDoublePolymorphicSpec :: SpecWith ()
intDoublePolymorphicSpec = do
  it "int int no polymoriphic" $ do
    intDoublePolymorphic (+1) `shouldBe` (2, 2.0)

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  intDoublePolymorphicSpec