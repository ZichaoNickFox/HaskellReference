{-# LANGUAGE OverloadedStrings #-}

module TextSpec (specs) where

import Data.Text
import Test.Hspec

specs :: SpecWith ()
specs = do
  describe "Data.Text" $ do
    it "pack on {- LANGUAGE OverloadedStrings -}" $ do
      pack "hello" `shouldBe` "hello"