{-# LANGUAGE TypeApplications #-}

module LanguageExtension.TypeApplicationsSpec (spec) where

import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "TypeApplications" $ do
    it "read" $ do
      (read "12" :: Int) `shouldBe` 12
      read @Int "12" `shouldBe` 12