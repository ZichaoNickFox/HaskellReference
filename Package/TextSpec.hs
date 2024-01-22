{-# LANGUAGE OverloadedStrings #-}

module Package.TextSpec (spec) where

import           Data.Text
import           Test.Hspec

spec :: SpecWith ()
spec = do
  it "pack on {- LANGUAGE OverloadedStrings -}" $ do
    pack "hello" `shouldBe` "hello"

main :: IO ()
main = hspec spec
