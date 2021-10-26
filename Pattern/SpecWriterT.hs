module Pattern.SpecWriterT (specs) where

import Util (shouldBeWhat)
import Control.Monad
import Data.Functor.Identity
import Prelude hiding (log)
import Test.Hspec

newtype WriterT m w a = Writer { runWriter :: m (a, w) } deriving (Show, Eq)

instance Functor (Writer m w) where
  fmap f (Writer m (a, w)) = Writer m (f a, w)
instance Monoid w => Applicative (Writer m w) where
  (<*>) (Writer m (f, w)) (Writer m (a, w')) = Writer m (f a, w <> w')
  pure a = Writer (a, mempty)
instance Monoid w => Monad (Writer w) where
  (>>=) (Writer m (a, w)) f = let Writer m (a', w') = f a in Writer m (a', w <> w')

tell :: w -> Writer m w ()
tell w = Writer m ((), w)

----------------------------------------------------------------------------------------------------

newtype Year = Year Int deriving (Show, Eq)
nextYear :: Year -> Year
nextYear (Year y) = Year (y + 1)

logOne :: Year -> Writer String Year
logOne x = do
  tell ("log : " ++ show x ++ ", ")
  return x

log :: Writer String Year
log = do
  logOne (Year 1998)
  logOne (Year 2015)
  logOne (Year 2021)

specLog :: SpecWith ()
specLog = do
  it "log" $ do
    runWriter log `shouldBe` (Year 2021, "log : Year 1998, log : Year 2015, log : Year 2021, ")
  
  it "fmap" $ do
    fmap (nextYear) log `shouldBe` Writer (Year 2022,"log : Year 1998, log : Year 2015, log : Year 2021, ")

  it "(<*>)" $ do
    Writer ((nextYear), "hi_") <*> Writer (Year 2021, "log_here") `shouldBe` Writer (Year 2022, "hi_log_here")
    Writer ((nextYear), "log_here") <*> Writer (Year 2021, "_hi") `shouldBe` Writer (Year 2022, "log_here_hi")

  it ">>=" $ do
    (Writer (Year 2021, "hello ") >>= \x -> Writer (nextYear x, "world")) `shouldBe` Writer (Year 2022, "hello world")

specs :: SpecWith ()
specs = do
  describe "Writer Specs" $ do
    specLog