{-# LANGUAGE TypeApplications #-}

module LanguageExtension.SpecTypeApplications (specs) where

import Test.Hspec

specs :: SpecWith ()
specs = do
  describe "TypeApplications" $ do
    it "read" $ do
      (read "12" :: Int) `shouldBe` 12
      read @Int "12" `shouldBe` 12