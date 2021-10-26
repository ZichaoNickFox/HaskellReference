{-# LANGUAGE RecordWildCards #-}

module LanguageExtension.RecordWildCardsSpec (spec) where

import Test.Hspec

data Person = Person {
  firstName :: String,
  lastName :: String
} deriving (Show, Eq)

leftNoWildCard :: Person -> String
leftNoWildCard (Person firstName lastName) = firstName ++ " " ++ lastName

leftWildCard :: Person -> String
leftWildCard Person {..} = firstName ++ " " ++ lastName

rightNoWildCard :: String -> String -> Person
rightNoWildCard firstName lastName = Person firstName lastName

rightWildCard :: String -> String -> Person
rightWildCard firstName lastName = Person {..}

specRecordWildCards :: SpecWith ()
specRecordWildCards = do
  it "RecordWildCards" $ do
    leftNoWildCard (Person "Zichao" "Liu") `shouldBe` "Zichao Liu"
    leftWildCard (Person "Zichao" "Liu") `shouldBe` "Zichao Liu"
    rightNoWildCard "Zichao" "Liu" `shouldBe` Person "Zichao" "Liu"
    rightWildCard "Zichao" "Liu" `shouldBe` Person "Zichao" "Liu"

spec :: SpecWith ()
spec = do
  describe "LanguageExtensionSpec" $ do
    specRecordWildCards