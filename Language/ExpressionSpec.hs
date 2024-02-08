module Language.ExpressionSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Test.Hspec

eraseDoSepc :: SpecWith ()
eraseDoSepc = do
  it "it + do, let + in" $ do
    let
      eraseDoSpec1 :: Maybe String
      eraseDoSpec1 = do
        a <- Just 1
        b <- Just 2
        return $ show (a + b)
    eraseDoSpec1 `shouldBe` Just "3"
  it "it - do, let - in" $
    let
      eraseDoSpec2 :: Maybe String
      eraseDoSpec2 = do
        a <- Just 1;
        b <- Just 2;
        return $ show (a + b)
    in eraseDoSpec2 `shouldBe` Just "3"

dontNeedDoSpec :: SpecWith ()
dontNeedDoSpec = do
  it "with do" $ do Just "a" `shouldBe` Just "a"
  it "without do" $ Just "a" `shouldBe` Just "a"

spec::SpecWith()
spec = do
  describe "eraseDoSpec" eraseDoSepc
  describe "dontNeedDoSpec" dontNeedDoSpec

main :: IO ()
main = hspec spec
