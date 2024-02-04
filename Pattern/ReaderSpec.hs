module Pattern.ReaderSpec (spec) where

import           Control.Monad.Reader
import           Data.Functor
import           Data.Functor.Identity
import           Test.Hspec

-- newtype Reader r a = Reader { runReader :: a } deriving (Show, Eq)

-- instance Functor (Reader r) where
--   fmap f (Reader r) = Reader (f r)
-- instance Applicative (Reader r)

newtype Ask a = Ask { content :: a } deriving (Show, Eq)
-- instance Functor Ask where
--   fmap f (Ask a) = Ask (f a)

tom :: Reader (Ask String) String
tom = do
  asking <- ask
  return (content asking ++ "This is Tom.");

jerry :: Reader (Ask String) String
jerry = do
  asking <- ask
  return (content asking ++ "This is Jerry.");

tomAndJerry :: Reader (Ask String) String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

tomAndJerrySpec :: SpecWith ()
tomAndJerrySpec = do
  it "Tom And Jerry" $ do
    runReader tomAndJerry (Ask "Who's there? ") `shouldBe`
      "Who's there? This is Tom.\nWho's there? This is Jerry."

  -- it "fmap" $ do
  --   fmap (++ " asked") (ReaderT (Ask "Who's there?") Identity) `shouldBe` ReaderT (Ask "Who's there? asked") Identity

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  tomAndJerrySpec

main :: IO ()
main = hspec spec
