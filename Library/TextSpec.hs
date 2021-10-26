{-# LANGUAGE OverloadedStrings #-}

module Library.TextSpec (spec) where

import Data.Text
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Data.Text" $ do
    it "pack on {- LANGUAGE OverloadedStrings -}" $ do
      pack "hello" `shouldBe` "hello"