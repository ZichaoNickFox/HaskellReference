module Language.SpecDo (specs) where

import Test.Hspec
import Data.Char
import Control.Monad
import Control.Applicative

specEraseDo :: Maybe String
specEraseDo = do
  a <- Just 1
  b <- Just 2
  Just "Nonsense"
  c <- Just 3
  return $ show (a + b + c)

specEraseDo' :: Maybe String
specEraseDo' = do {
  a <- Just 1;
  b <- Just 2;
  Just "Nonsense";
  c <- Just 3;
  return $ show (a + b + c) }

specEraseDo'' :: Maybe String
specEraseDo'' =
  Just 1 >>= \a ->
  Just 2 >>= \b ->
  Just "Nonesense" >>
  Just 3 >>= \c ->
  return $ show (a + b + c)

specDontNeedDo :: Maybe String
specDontNeedDo = do Just "a"

specDontNeedDo' :: Maybe String
specDontNeedDo' = Just "a"

specNeedDo :: Maybe String
specNeedDo = do
  Just "a"
  Just "b"
  Just "c"

specNeedDo' :: Maybe String
specNeedDo' = do {
  Just "a";
  Just "b";
  Just "c";}

specNeedDo'' :: Maybe String
specNeedDo'' =
  Just "a"

specNeedDo''' :: Maybe String
specNeedDo''' =
  Just "a" >>
  Just "b" >>
  Just "c"

specs::SpecWith()
specs =
  describe "DoSpec" $ do
    it "Erase Do" $ do
      specEraseDo `shouldBe` Just "6"
      specEraseDo' `shouldBe` Just "6"
      specEraseDo'' `shouldBe` Just "6"

    it "Dont Need Do" $ do
      specDontNeedDo `shouldBe` Just "a"
      specDontNeedDo' `shouldBe` Just "a"

    it "Need Do" $ do
      specNeedDo `shouldBe` Just "c"
      specNeedDo' `shouldBe` Just "c"
      specNeedDo'' `shouldBe` Just "a"
      specNeedDo''' `shouldBe` Just "c"