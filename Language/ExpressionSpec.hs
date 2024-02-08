module Language.ExpressionSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Test.Hspec

doSpec :: SpecWith ()
doSpec = do
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

spec::SpecWith()
spec = do
  describe "doSpec" doSepc

main :: IO ()
main = hspec spec
