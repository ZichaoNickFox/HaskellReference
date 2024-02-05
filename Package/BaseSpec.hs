{-# LANGUAGE DeriveDataTypeable #-}
module Package.BaseSpec where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.IORef

import           Data.Monoid                           as Monoid
import           Data.Semigroup                        as Semigroup
import           Data.Typeable
import           Distribution.PackageDescription.Check (CheckPackageContentOps (doesFileExist))
import           Distribution.Simple.Utils             (doesExecutableExist)
import           System.Environment
import           Test.Hspec

preludeSpec :: SpecWith ()
preludeSpec = do
  it "Prelude" $ do
    flip (++) "hello" "world" `shouldBe` "worldhello"
    flip (/) 3 6 `shouldBe` 2

    unlines ["a"] `shouldBe` "a\n"
    unlines ["a", "b"] `shouldBe` "a\nb\n"
    unlines ["a", "\n", "b"] `shouldBe` "a\n\n\nb\n"

    const "a" 1 `shouldBe` "a"
    const ['a', 'b'] () `shouldBe` ['a', 'b']

    isDigit '1' `shouldBe` True
    isDigit 'a' `shouldBe` False

    fmap show (Just 1) `shouldBe` Just "1"
    (+1) <$> [1, 2, 3] `shouldBe` [2, 3, 4]

    when True (Just ()) `shouldBe` (Just ())
    when False (Just ()) `shouldBe` (return ())

    unless False (Just ()) `shouldBe` (Just ())
    unless True (Just ()) `shouldBe` (return ())

    void (Just 1) `shouldBe` (Just ())
    void Nothing `shouldBe` Nothing

    (read "12" :: Int) `shouldBe` 12

    readFile "Data/Test.txt" >>= (\s -> s `shouldBe` "Hello World")

    writeFile "Data/Test.txt" "ABC"
    >> readFile "Data/Test.txt" >>= (\s -> s `shouldBe` "ABC")
    >> writeFile "Data/Test.txt" "Hello World"

dataEitherSpec :: SpecWith ()
dataEitherSpec = do
  it "Data.Either" $ do
    (Left 1 :: Either Int String) `shouldBe` (Left 1 :: Either Int String)
    (Right "1" :: Either Int String) `shouldBe` (Right "1" :: Either Int String)

----------------------------------------------------------------------------------------------------

dataTupleSpec :: SpecWith ()
dataTupleSpec = do
  it "Data.Tuple" $ do
    fst (1, 2) `shouldBe` 1
    snd (1, 2) `shouldBe` 2

----------------------------------------------------------------------------------------------------

ioRefSpec :: SpecWith ()
ioRefSpec = do
  it "IORef" $ do
    r <- newIORef 0
    writeIORef r 1
    v <- readIORef r
    show v `shouldBe` "1"
    v <- atomicModifyIORef r (\x -> (x, x + 10))
    show v `shouldBe` "11"

----------------------------------------------------------------------------------------------------

systemEnvironmentSpec :: SpecWith ()
systemEnvironmentSpec = do
  it "environment interfaces" $ do
    setEnv "TestHaskellEnv" "version0.1"
    getEnv "TestHaskellEnv" >>= (\s -> s `shouldBe` "version0.1")
    lookupEnv "TestHaskellEnv" >>= (\m -> m `shouldBe` (Just "version0.1"))
    unsetEnv "TestHaskellEnv"
    -- getEnv "TestHaskellEnv" CRASH
    lookupEnv "TestHaskellEnv" >>= (\m -> m `shouldBe` Nothing)

----------------------------------------------------------------------------------------------------

data Person = Person { name :: String, age :: Int } deriving (Typeable)

zichaoLiu :: Person
zichaoLiu = Person "Zichao Liu" 30

typeableSpec :: SpecWith ()
typeableSpec = do
  it "typeable spec" $ do
    let typeRep = typeOf (undefined :: Person) in typeRep `shouldBe` typeOf zichaoLiu
    let showTypeRep = show $ typeOf zichaoLiu in showTypeRep `shouldBe` "Person"
    let showTypeRepCon = show $ typeRepTyCon $ typeOf zichaoLiu in showTypeRepCon `shouldBe` "Person"
    tyConName (typeRepTyCon $ typeOf zichaoLiu) `shouldBe` "Person"
    tyConModule (typeRepTyCon $ typeOf zichaoLiu) `shouldBe` "Package.BaseSpec"
    tyConPackage (typeRepTyCon $ typeOf zichaoLiu) `shouldBe` "main"

----------------------------------------------------------------------------------------------------

-- controlMonadSpec :: SpecWith ()
-- controlMonadSpec = do
--   describe "Control.Monad" $ do
--     it "liftIO" $ do

-- // ANCHOR[id=Identity] Identity
-- Identity is a functor satisfied : f <$> Identity a = Identity (f a)
identitySpec :: SpecWith ()
identitySpec = do
  it "Identity" $ fmap (+1) (runIdentity 1) `shouldBe` Identity 2

-- // ANCHOR[id=Const] Const
-- Const is a functor satisfied : f <$> Const a = Const a
constSpec :: SpecWith ()
constSpec = do
  it "Const" $ fmap (+1) (Const 1) `shouldBe` Const 1

monoidSpec :: SpecWith ()
monoidSpec = do
  describe "monoid" $ do
    it "Alt" $ Alt (Just 12) <> Alt (Just 24) `shouldBe` Alt (Just 12)
    it "Alt" $ Alt Nothing <> Alt (Just 12) `shouldBe` Alt (Just 12)

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Semigroup.html
semigroupSpec :: SpecWith ()
semigroupSpec = do
  describe "semigroup" $ do
    it "array" $ [1, 2, 3] <> [4, 5, 6] `shouldBe` [1, 2, 3, 4, 5, 6]
    it "Min" $ Min 1 <> Min 2 <> Min 3 `shouldBe` Min 1
  -- // ANCHOR[id=First] First
    it "First" $ Semigroup.First Nothing <> Semigroup.First (Just 1) `shouldBe` Semigroup.First Nothing
    it "Last" $ Semigroup.Last Nothing <> Semigroup.Last (Just 1) `shouldBe` Semigroup.Last (Just 1)
    it "Dual" $ Dual "hello" <> Dual "world" `shouldBe` Dual "worldhello"
    -- it "Endo" TODO:
    it "All" $ All True <> All False `shouldBe` All False
    it "Any" $ Any True <> Any False `shouldBe` Any True
    it "Sum" $ Sum 1 <> Sum 2 <> mempty `shouldBe` Sum 3
    it "Product" $ Product 2 <> Product 3 `shouldBe` Product 6
    -- it "diff" TODO:

spec::SpecWith ()
spec = do
  preludeSpec
  dataEitherSpec
  dataTupleSpec
  ioRefSpec
  systemEnvironmentSpec
  typeableSpec
  -- controlMonadSpec
  identitySpec
  constSpec
  monoidSpec
  semigroupSpec

main :: IO ()
main = hspec spec
