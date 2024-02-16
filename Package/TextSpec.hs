{-# LANGUAGE OverloadedStrings #-}

module Package.TextSpec (spec) where

import           Data.Char
import           Data.MonoTraversable
import           Data.Text            hiding (toUpper)
import           Prelude              hiding (map)
import           Test.Hspec

spec :: SpecWith ()
spec = do
  it "pack on {- LANGUAGE OverloadedStrings -}" $ do
    pack "hello" `shouldBe` "hello"
  it "map" $
    map toUpper "hello world" `shouldBe` "HELLO WORLD"

main :: IO ()
main = hspec spec
