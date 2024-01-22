module Language.DoSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Test.Hspec

eraseDoSpec1 :: Maybe String
eraseDoSpec1 = do
  a <- Just 1
  b <- Just 2
  Just "Nonsense"
  c <- Just 3
  return $ show (a + b + c)

eraseDoSpec2 :: Maybe String
eraseDoSpec2 = do {
  a <- Just 1;
  b <- Just 2;
  Just "Nonsense";
  c <- Just 3;
  return $ show (a + b + c) }

eraseDoSpec3 :: Maybe String
eraseDoSpec3 =
  Just 1 >>= \a ->
  Just 2 >>= \b ->
  Just "Nonesense" >>
  Just 3 >>= \c ->
  return $ show (a + b + c)

eraseDoSepc :: SpecWith ()
eraseDoSepc = do
  it "Erase Do" $ do
    eraseDoSpec1 `shouldBe` Just "6"
    eraseDoSpec2 `shouldBe` Just "6"
    eraseDoSpec3 `shouldBe` Just "6"

----------------------------------------------------------------------------------------------------

dontNeedDoSpec1 :: Maybe String
dontNeedDoSpec1 = do Just "a"

dontNeedDoSpec2 :: Maybe String
dontNeedDoSpec2 = Just "a"

dontNeedDoSpec :: SpecWith ()
dontNeedDoSpec = do
  it "Dont Need Do" $ do
    dontNeedDoSpec1 `shouldBe` Just "a"
    dontNeedDoSpec2 `shouldBe` Just "a"

----------------------------------------------------------------------------------------------------

needDoSpec1 :: Maybe String
needDoSpec1 = do
  Just "a"
  Just "b"
  Just "c"

needDoSpec2 :: Maybe String
needDoSpec2 = do {
  Just "a";
  Just "b";
  Just "c";}

needDoSpec3 :: Maybe String
needDoSpec3 =
  Just "a"

needDoSpec4 :: Maybe String
needDoSpec4 =
  Just "a" >>
  Just "b" >>
  Just "c"

needDoSpec :: SpecWith ()
needDoSpec = do
  it "Need Do" $ do
    needDoSpec1 `shouldBe` Just "c"
    needDoSpec2 `shouldBe` Just "c"
    needDoSpec3 `shouldBe` Just "a"
    needDoSpec4 `shouldBe` Just "c"

----------------------------------------------------------------------------------------------------

spec::SpecWith()
spec = do
  eraseDoSepc
  dontNeedDoSpec
  needDoSpec

main :: IO ()
main = hspec spec
