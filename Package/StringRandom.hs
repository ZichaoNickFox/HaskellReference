{-# LANGUAGE OverloadedStrings #-}
module Package.StringRandom where

import           Test.Hspec
import           Text.StringRandom

spec :: SpecWith ()
spec = do
  it "stringRandomIO" $ do
    ymd <- stringRandomIO "20\\d\\d-(1[0-2]|0[1-9])-(0[1-9]|1\\d|2[0-8])"
    print ymd

main :: IO ()
main = hspec spec
