{-# LANGUAGE NoImplicitPrelude #-}

module LanguageExtension.NoImplicitPreludeSpec (spec) where

import qualified Prelude    as P
import           Test.Hspec

data Maybe a = Nothing | Just a deriving (P.Show)

spec :: SpecWith ()
spec = do
  it "show -> P.show. $ -> P.$" P.$ do
    P.show (Just 1) `shouldBe` "Just 1"

main :: P.IO ()
main = hspec spec
