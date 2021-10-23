module ReaderSpec (specs) where

import Control.Monad.Reader
import Test.Hspec

specTomAndJerry :: SpecWith ()
specTomAndJerry = do
  it "Tom And Jerry" $ do
    runReader tomAndJerry "Who's there? " `shouldBe`
      "Who's there? This is Tom.\nWho's there? This is Jerry."

tom :: Reader String String
tom = do
  env <- ask
  return (env ++ "This is Tom.");

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ "This is Jerry.");

tomAndJerry :: Reader String String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

----------------------------------------------------------------------------------------------------

specs :: SpecWith ()
specs = do
  describe "Reader Spec" $ do
    specTomAndJerry
