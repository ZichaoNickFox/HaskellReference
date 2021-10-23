module Library.BaseSpec where

import Test.Hspec
import Data.Char
import Control.Monad
import Control.Applicative

specs::SpecWith()
specs = do
  describe "Base" $ do
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

    describe "Data.Char" $ do
      it "isDigit" $ do
        isDigit '1' `shouldBe` True
        isDigit 'a' `shouldBe` False

    describe "Data.Functor" $ do
      it "fmap, <$>" $ do
        fmap show (Just 1) `shouldBe` Just "1"
        (+1) <$> [1, 2, 3] `shouldBe` [2, 3, 4]

    describe "Control.Monad" $ do
      it "unless" $ do
        unless False (Just ()) `shouldBe` (Just ())
        unless True (Just ()) `shouldBe` (return ())
      
      it "void" $ do
        void (Just 1) `shouldBe` (Just ())
        void Nothing `shouldBe` Nothing