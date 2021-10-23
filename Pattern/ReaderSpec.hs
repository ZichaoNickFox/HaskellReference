module Pattern.ReaderSpec (specs) where

import Test.Hspec
import Control.Monad.Reader

newtype Ask = Ask { content :: String } deriving (Show, Eq)

tom :: Reader Ask String
tom = do
  asking <- ask
  return (content asking ++ "This is Tom.");

jerry :: Reader Ask String
jerry = do
  asking <- ask
  return (content asking ++ "This is Jerry.");

tomAndJerry :: Reader Ask String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

specTomAndJerry :: SpecWith ()
specTomAndJerry = do
  it "Tom And Jerry" $ do
    runReader tomAndJerry (Ask "Who's there? ") `shouldBe`
      "Who's there? This is Tom.\nWho's there? This is Jerry."

----------------------------------------------------------------------------------------------------

specs :: SpecWith ()
specs = do
  describe "Reader Spec" $ do
    specTomAndJerry
