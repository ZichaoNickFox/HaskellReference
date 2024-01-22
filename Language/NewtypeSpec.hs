{-# LANGUAGE OverloadedStrings #-}

module Language.NewtypeSpec (spec) where

import           Data.Text
import           Test.Hspec

newtype Age = Age Int deriving (Show, Eq)
newtype Param a b = Param (Either a b) deriving (Show, Eq)
newtype Record = Record { getInt :: Int } deriving (Show, Eq)
newtype AppHostName = AppHostName Text deriving (Show, Eq)
newtype AppPort = AppPort Int deriving (Show, Eq)
newtype AppBaseUrl = AppBaseUrl Text deriving (Show, Eq)

spec :: SpecWith ()
spec = do
  it "newtype" $ do
    Age 1 `shouldBe` Age 1
    (Param (Left 1 :: Either Int String)) `shouldBe` (Param (Left 1 :: Either Int String))
    Record 2021 `shouldBe` Record 2021
    AppHostName "nickfox" `shouldBe` AppHostName "nickfox"
    AppPort 8080 `shouldBe` AppPort 8080
    AppBaseUrl "127.0.0.1" `shouldBe` AppBaseUrl "127.0.0.1"

main :: IO ()
main = hspec spec
