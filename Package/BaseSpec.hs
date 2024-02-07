{-# LANGUAGE DeriveDataTypeable #-}
module Package.BaseSpec where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.IORef
import           Data.Maybe
import           Data.Monoid                           as Monoid
import           Data.Semigroup                        as Semigroup
import           Data.Typeable
import           Distribution.PackageDescription.Check (CheckPackageContentOps (doesFileExist))
import           Distribution.Simple.Utils             (doesExecutableExist)
import           System.Environment
import           Test.Hspec
import           Text.Read

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

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Maybe.html
maybeSpec :: SpecWith ()
maybeSpec = do
  it "readMaybe" $ (readMaybe "1" :: Maybe Int) `shouldBe` Just 1
  it "readMaybe" $ (readMaybe "zichao" :: Maybe Int) `shouldBe` Nothing
  it "maybe" $ maybe 2 (+1) (Just 3) `shouldBe` 4
  it "maybe" $ maybe 2 (+1) Nothing `shouldBe` 2
  it "maybe + readMaybe" $ maybe 0 (+1) (readMaybe "2" :: Maybe Int) `shouldBe` 3
  it "maybe + readMaybe" $ maybe 0 (+1) (readMaybe "" :: Maybe Int) `shouldBe` 0
  it "maybe + readMaybe" $ maybe "" show (Just 5) `shouldBe` "5"
  it "maybe + readMaybe" $ maybe "" (show :: Int -> String) Nothing `shouldBe` ""
  it "isJust" $ isJust (Just 1) `shouldBe` True
  it "isJust" $ isJust (Just Nothing) `shouldBe` True
  it "isJust" $ isJust Nothing `shouldBe` False
  it "isNothing" $ isNothing (Just ()) `shouldBe` False
  it "isNothing" $ isNothing Nothing `shouldBe` True
  it "fromJust" $ fromJust (Just 1) `shouldBe` 1
  it "fromJust" $ evaluate (fromJust Nothing) `shouldThrow` anyErrorCall
  it "fromMaybe" $ fromMaybe "" (Just "hello") `shouldBe` "hello"
  it "fromMaybe" $ fromMaybe "world" Nothing `shouldBe` "world"
  it "fromMaybe + readMaybe" $ fromMaybe 0 (readMaybe "") `shouldBe` 0
  it "fromMaybe + readMaybe" $ fromMaybe 0 (readMaybe "1") `shouldBe` 1
  it "listToMaybe" $ listToMaybe [] `shouldBe` (Nothing :: Maybe Int)
  it "listToMaybe" $ listToMaybe [1] `shouldBe` Just 1
  it "listToMaybe" $ listToMaybe [1, 2, 3] `shouldBe` Just 1
  it "maybeToList" $ maybeToList (Just 1) `shouldBe` [1]
  it "maybeToList" $ maybeToList (Just [1, 2]) `shouldBe` [[1, 2]]
  it "maybeToList" $ maybeToList Nothing `shouldBe` ([] :: [Int])
  it "catMaybes" $ catMaybes [Just 1, Nothing, Just 3] `shouldBe` [1, 3]
  it "mapMaybe" $ mapMaybe id [Just 1, Nothing] `shouldBe` [1]

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
  it "Alt" $ Alt (Just 12) <> Alt (Just 24) `shouldBe` Alt (Just 12)
  it "Alt" $ Alt Nothing <> Alt (Just 12) `shouldBe` Alt (Just 12)

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Semigroup.html
semigroupSpec :: SpecWith ()
semigroupSpec = do
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
  describe "preludeSpec" preludeSpec
  describe "maybeSpec" maybeSpec
  describe "dataEitherSpec" dataEitherSpec
  describe "dataTupleSpec" dataTupleSpec
  describe "ioRefSpec" ioRefSpec
  describe "systemEnvironmentSpec" systemEnvironmentSpec
  describe "typeableSpec" typeableSpec
  -- describe "controlMonadSpec" controlMonadSpec
  describe "identitySpec" identitySpec
  describe "constSpec" constSpec
  describe "monoidSpec" monoidSpec
  describe "semigroupSpec" semigroupSpec

main :: IO ()
main = hspec spec
