{-# LANGUAGE TypeApplications #-}

module LanguageExtension.TypeApplicationsSpec (spec) where

import           Test.Hspec

spec :: SpecWith ()
spec = do
    -- https://www.haskellforall.com/2015/10/polymorphism-for-dummies.html
    it "read" $ do
      (read "12" :: Int) `shouldBe` 12
      (read @Int :: String -> Int) "12" `shouldBe` 12

    it "[]" $ do
      2 : ([] @Int) `shouldBe` [2]

main :: IO ()
main = hspec spec
