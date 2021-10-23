{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module StateSpec (specs) where

import Control.Monad
import Data.Functor
import Prelude
import Test.Hspec

type S = Int
type A = Char

newtype State s a = State { runState :: s -> (a,s) }

get :: State as as
get = State (\as -> (as, as))

put :: s -> State s ()
put ss = State (\s -> ((), ss))

modify :: (s -> s) -> State s ()
modify f = State(\s -> ((), f s))

gets :: (s -> a) -> State s a
gets f = State (\s -> (f s, s))

evalState :: State s a -> s -> a
evalState st s = fst $ runState st s

execState :: State s a -> s -> s
execState st s = snd $ runState st s

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f st = State (\s -> let (a, s1) = runState st s
                           in (f a, s))

instance Applicative (State s) where
  (<*>) :: (State s (a -> b)) -> State s a -> State s b
  (<*>) app st = State (\s -> let (f, s1) = runState app s
                                  (a, s2) = runState st s1
                              in (f a, s2))
  pure :: a -> State s a
  pure a = State (\s -> (a, s))
  
instance Monad (State s) where
  (>>) :: (State s a) -> (State s b) -> (State s b)
  (>>) st1 st2 = State (\s -> let (a, s1) = runState st1 s
                                  (a', s2) = runState st2 s1
                              in (a', s2))
  (>>=) :: (State s a) -> (a -> State s b) -> (State s b)
  (>>=) st1 fst2 = State (\s -> let (a, s1) = runState st1 s
                                    st2 = fst2 a
                                    (a1, s2) = runState st2 s1
                                in (a1, s2))

specs :: SpecWith ()
specs = do
  describe "StateSpec" $ do
    it "return' : set value" $ do
      runState (return 'a') 1 `shouldBe` ('a', 1)
      (runState :: State S A->S->(A,S)) ((return :: A -> State S A) 'a'::State S A) 1 `shouldBe` ('a', 1)

    it "get : set value, state same" $ do
      runState get 'a' `shouldBe` ('a', 'a')
      runState (get::State A A) 'a' `shouldBe` ('a', 'a')
    
    it "put : set value (), set state" $ do
      runState (put 5) 1 `shouldBe` ((), 5)
      runState ((put :: S -> State S ()) 5 :: State S ()) 1 `shouldBe` ((), 5)

    it "modify : set value (), modify state" $ do
      runState (modify (+10)) 1 `shouldBe` ((), 11)

    it "gets : get, modify value, set state" $ do
      runState (gets (+10)) 1 `shouldBe` (11, 1)

    it "evalState : take value" $ do
      evalState (gets (+10)) 1 `shouldBe` 11

    it "execState : take state" $ do
      execState (gets (+10)) 1 `shouldBe` 1
    
    it "combination" $ do
      runState (do {put 5; return 'a'}) 1 `shouldBe` ('a', 5)
      runState (put 5  >> return 'a') 1 `shouldBe` ('a', 5)

      runState (get >>= \x -> put (x + 1) >> return x) 1 `shouldBe` (1, 2)

      runState (do {x <- get; put (x - 1); get}) 1 `shouldBe` (0, 0)
      
      runState (do {get; return "a"; return "b"}) 1 `shouldBe` ("b", 1)
