module Language.ExpressionSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Test.Hspec

doSpec :: SpecWith ()
doSpec = do
  it "it + do, let - in" $ do
    let getMaybe :: Maybe String
        getMaybe = do
          a <- Just 1
          b <- Just 2
          return $ show (a + b)
    getMaybe `shouldBe` Just "3"
  it "it - do, let + in" $
    let getMaybe :: Maybe String
        getMaybe = do
          a <- Just 1
          b <- Just 2
          return $ show (a + b)
    in  getMaybe `shouldBe` Just "3"
  it "<-" $ do
    let getIOMaybe :: IO (Maybe String)
        getIOMaybe = return (Just "3")
    Just s <- getIOMaybe
    s `shouldBe` "3"

spec::SpecWith()
spec = do
  describe "doSpec" doSpec

main :: IO ()
main = hspec spec
