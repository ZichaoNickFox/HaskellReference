module WriterSpec (specs) where

import Util (shouldBeWhat)
import Control.Monad
import Data.Functor.Identity
import Prelude hiding (log)
import Test.Hspec

newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show, Eq)

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)
instance Monoid w => Applicative (Writer w) where
  (<*>) (Writer (f, w)) (Writer (a, w')) = Writer (f a, w <> w')
  pure a = Writer (a, mempty)
instance Monoid w => Monad (Writer w) where
  (>>=) (Writer (a, w)) f = let Writer (a', w') = f a in Writer (a', w <> w')

tell :: w -> Writer w ()
tell w = Writer ((), w)

logOne :: Int -> Writer String Int
logOne x = do
  tell ("log : " ++ show x ++ ", ")
  return x

log :: Writer String Int
log = do
  logOne 1
  logOne 2
  logOne 3

specLog :: SpecWith ()
specLog = do
  it "log" $ do
    runWriter log `shouldBe` (3, "log : 1, log : 2, log : 3, ")
  
  it "fmap" $ do
    fmap (*3) log `shouldBe` Writer (9,"log : 1, log : 2, log : 3, ")

  it "(<*>)" $ do
    Writer ((*2), "hi_") <*> Writer (11, "log_here") `shouldBe` Writer (22, "hi_log_here")
    Writer ((*2), "log_here") <*> Writer (11, "_hi") `shouldBe` Writer (22, "log_here_hi")

  it ">>=" $ do
    (Writer (2, "hello ") >>= \x -> Writer (2 * x, "world")) `shouldBe` Writer (4, "hello world")

specs :: SpecWith ()
specs = do
  describe "Writer Specs" $ do
    specLog