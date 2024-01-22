module Pattern.WriterSpec (spec) where

import           Control.Monad
import           Data.Functor.Identity
import           Prelude               hiding (log)
import           Test.Hspec
import           Util                  (shouldBeWhat)

newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show, Eq)

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)
instance Monoid w => Applicative (Writer w) where
  (<*>) (Writer (f, w)) (Writer (a, w')) = Writer (f a, w <> w')
  pure a = Writer (a, mempty)
instance Monoid w => Monad (Writer w) where
  (>>=) (Writer (a, w)) f = let Writer (a', w') = f a in Writer (a', w <> w')

execWriter :: Writer w a -> w
execWriter writer = let (a, w) = runWriter writer in w

tell :: w -> Writer w ()
tell w = Writer ((), w)

listen :: Writer w a -> Writer w (a, w)
listen (Writer (a, w)) = Writer ((a, w), w)

listens :: (w -> b) -> Writer w a -> Writer w (a, b)
listens fw (Writer (a, w)) = Writer ((a, fw w), w)

pass :: Writer w (a, w -> w') -> Writer w' a
pass (Writer ((a, fw), w)) = Writer (a, fw w)

censor :: (w -> w) -> Writer w a -> Writer w a
censor fw (Writer (a, w)) = Writer (a, fw w)

----------------------------------------------------------------------------------------------------

newtype Year = Year Int deriving (Show, Eq)
nextYear :: Year -> Year
nextYear (Year y) = Year (y + 1)

tellSpec :: Writer String Year
tellSpec = do
  tell ("log " ++ show (Year 1998) ++ ", ")
  tell ("log " ++ show (Year 2015) ++ ", ")
  tell ("log " ++ show (Year 2021))
  return (Year 2021)

censorSpec :: Writer [String] ()
censorSpec = do
  censor eliminateBad all where
    all :: Writer [String] ()
    all = do
      tell ["good"]
      tell ["excellent"]
      tell ["bad"]
      tell ["soso"]
      tell ["good"]
      tell ["bad"]
    eliminateBad :: [String] -> [String]
    eliminateBad = filter (\x -> (x /= "bad"))

logSpec :: SpecWith ()
logSpec = do
  it "tell" $ do
    runWriter tellSpec `shouldBe` (Year 2021, "log Year 1998, log Year 2015, log Year 2021")

  it "censor" $ do
    runWriter censorSpec `shouldBe` ((), ["good", "excellent", "soso", "good"])

  it "fmap" $ do
    fmap (nextYear) tellSpec `shouldBe` Writer (Year 2022,"log Year 1998, log Year 2015, log Year 2021")

  it "(<*>)" $ do
    Writer ((nextYear), "hi_") <*> Writer (Year 2021, "log_here") `shouldBe` Writer (Year 2022, "hi_log_here")
    Writer ((nextYear), "log_here") <*> Writer (Year 2021, "_hi") `shouldBe` Writer (Year 2022, "log_here_hi")

  it ">>=" $ do
    (Writer (Year 2021, "hello ") >>= \x -> Writer (nextYear x, "world")) `shouldBe` Writer (Year 2022, "hello world")

spec :: SpecWith ()
spec = do
  logSpec

main :: IO ()
main = hspec spec
