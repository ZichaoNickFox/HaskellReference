module Library.PreludeSpec where

import Test.Hspec
import Data.Char
import Control.Monad
import Control.Applicative

spec::SpecWith()
spec = do
  describe "Prelude" $ do
    it "flip" $ do
      flip (++) "hello" "world" `shouldBe` "worldhello" 
      flip (/) 3 6 `shouldBe` 2

    it "unlines" $ do
      unlines ["a"] `shouldBe` "a\n"
      unlines ["a", "b"] `shouldBe` "a\nb\n"
      unlines ["a", "\n", "b"] `shouldBe` "a\n\n\nb\n"

    it "const" $ do
      const "a" 1 `shouldBe` "a"
      const ['a', 'b'] () `shouldBe` ['a', 'b']

    it "isDigit" $ do
      isDigit '1' `shouldBe` True
      isDigit 'a' `shouldBe` False

    it "fmap, <$>" $ do
      fmap show (Just 1) `shouldBe` Just "1"
      (+1) <$> [1, 2, 3] `shouldBe` [2, 3, 4]

    it "unless" $ do
      unless False (Just ()) `shouldBe` (Just ())
      unless True (Just ()) `shouldBe` (return ())
    
    it "void" $ do
      void (Just 1) `shouldBe` (Just ())
      void Nothing `shouldBe` Nothing

    it "read" $ do
      (read "12" :: Int) `shouldBe` 12