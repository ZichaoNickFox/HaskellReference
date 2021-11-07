{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Pattern.StateTSpec (spec) where

import Control.Arrow
import Control.Monad
import Data.Functor
import Prelude
import Test.Hspec

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap :: Functor m => (a -> b) -> StateT s m a -> StateT s m b
  fmap f st = StateT (\s -> (first f) <$> runStateT st s)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT (\s -> pure (a, s))

  (<*>) :: Monad m => (StateT s m (a -> b)) -> StateT s m a -> StateT s m b
  StateT f <*> StateT st = StateT (\s -> (f s >>= \(f, s1) -> st s1 >>= \(a, s2) -> return (f a, s2)))

instance Monad m => Monad (StateT s m) where
  (>>) :: (StateT s m a) -> (StateT s m b) -> (StateT s m b)
  StateT st1 >> StateT st2 = StateT (\s -> st1 s >>= \(a, s1) -> st2 s1 >>= \(a', s2) -> return (a', s2))

  (>>=) :: (StateT s m a) -> (a -> StateT s m b) -> (StateT s m b)
  StateT st1 >>= fst2 = StateT (\s -> st1 s >>= \(a, s1) -> (runStateT (fst2 a) s1))

getT :: Applicative m => StateT s m s
getT = StateT (\s -> pure (s, s))

putT :: Applicative m => s -> StateT s m ()
putT s = StateT (\_ -> pure ((), s))

modifyT :: Applicative m => (s -> s) -> StateT s m ()
modifyT f = StateT(\s -> pure ((), f s))

getsT :: Applicative m => (s -> a) -> StateT s m a
getsT f = StateT (\s -> pure (f s, s))

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT st s = fst <$> runStateT st s

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT st s = snd <$> runStateT st s

----------------------------------------------------------------------------------------------------

spec :: SpecWith ()
spec = do
  describe "StateTSpec" $ do
    it "return' : set value" $ do
      runStateT (return 'a') (1::Int) `shouldBe` Just ('a', 1)

    it "getT : set value, state same" $ do
      runStateT getT 'a' `shouldBe` Just ('a', 'a')
    
    it "putT : set value (), set state" $ do
      runStateT (putT 5) 1 `shouldBe` Just ((), 5)

    it "modifyT : set value (), modify state" $ do
      runStateT (modifyT (+10)) 1 `shouldBe` Just ((), 11)

    it "getsT : get, modify value, set state" $ do
      runStateT (getsT (+10)) 1 `shouldBe` Just (11, 1)

    it "evalStateT : take value" $ do
      evalStateT (getsT (+10)) 1 `shouldBe` Just 11

    it "execStateT : take state" $ do
      execStateT (getsT (+10)) 1 `shouldBe` Just 1
    
    it "combination" $ do
      runStateT (do {putT 5; return 'a'}) 1 `shouldBe` Just ('a', 5)
      runStateT (putT 5  >> return 'a') 1 `shouldBe` Just ('a', 5)
      runStateT (getT >>= \x -> putT (x + 1) >> return x) 1 `shouldBe` Just (1, 2)
      runStateT (do {x <- getT; putT (x - 1); getT}) 1 `shouldBe` Just (0, 0)
      runStateT (do {getT; return "a"; return "b"}) 1 `shouldBe` Just ("b", 1)

    it "StateT whatever m type" $ do
      runStateT (return 'a') (1::Int) `shouldBe` Just ('a', 1)
      runStateT (return 'a') (1::Int) `shouldBe` (Right ('a', 1) :: Either Int (Char, Int))