{-# LANGUAGE DeriveDataTypeable #-}
module Package.BaseSpec where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
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
import           System.IO
import           Test.Hspec
import           Text.Read
import           Util                                  (shouldBeWhat)

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

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Either.html
eitherSpec :: SpecWith ()
eitherSpec = do
  it "fmap ignore Left" $ fmap (*2) (Left "foo" :: Either String Int) `shouldBe` (Left "foo")
  it "fmap Right" $ fmap (*2) (Right 1 :: Either String Int) `shouldBe` (Right 2)
  it "either" $ either length (*2) (Left "foo") `shouldBe` 3
  it "either" $ either length (*2) (Right 3 :: Either String Int) `shouldBe` 6
  it "lefts" $ lefts [Left "a", Right 1, Left "b", Right 2] `shouldBe` ["a", "b"]
  it "rights" $ rights [Left "a", Right 1, Left "b", Right 2] `shouldBe` [1, 2]
  it "isLeft" $ isLeft (Left 1) `shouldBe` True
  it "isLeft" $ isLeft (Right 1) `shouldBe` False
  it "isRight" $ isRight (Left 1) `shouldBe` False
  it "isRight" $ isRight (Right 1) `shouldBe` True

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

dataTupleSpec :: SpecWith ()
dataTupleSpec = do
  it "Data.Tuple" $ do
    fst (1, 2) `shouldBe` 1
    snd (1, 2) `shouldBe` 2

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-IORef.html
ioRefSpec :: SpecWith ()
ioRefSpec = do
  it "newIORef readIORef" $ do
    r <- newIORef 0
    v <- readIORef r
    v `shouldBe` 0
  it "writeIORef" $ do
    r <- newIORef 0
    writeIORef r 1
    v <- readIORef r
    v `shouldBe` 1
  it "atomicWriteIORef" $ do
    r <- newIORef 0
    atomicWriteIORef r 2
    v <- readIORef r
    v `shouldBe` 2
  it "modifyIORef" $ do
    r <- newIORef 0
    modifyIORef r (+3)
    v <- readIORef r
    v `shouldBe` 3
  it "atomicModifyIORef" $ do
    r <- newIORef 0
    atomicModifyIORef r $ \v -> (v + 4, ())
    v <- readIORef r
    v `shouldBe` 4

systemEnvironmentSpec :: SpecWith ()
systemEnvironmentSpec = do
  it "environment interfaces" $ do
    setEnv "TestHaskellEnv" "version0.1"
    getEnv "TestHaskellEnv" >>= (\s -> s `shouldBe` "version0.1")
    lookupEnv "TestHaskellEnv" >>= (\m -> m `shouldBe` (Just "version0.1"))
    unsetEnv "TestHaskellEnv"
    -- getEnv "TestHaskellEnv" CRASH
    lookupEnv "TestHaskellEnv" >>= (\m -> m `shouldBe` Nothing)

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

endoSpec :: SpecWith ()
endoSpec = do
  -- like += -= in c++
  it "endo" $ do
    let compute = Endo ("hello " ++ )
    appEndo compute "world" `shouldBe` "hello world"
  it "endo" $ do
    let compute = Endo (++ "!")
    appEndo compute "world" `shouldBe` "world!"
  it "endo" $ do
    let compute = Endo ("hello " ++ ) <> Endo (++ "!")
    appEndo compute "world" `shouldBe` "hello world!"

applicativeSpec :: SpecWith ()
applicativeSpec = do
  it "liftA" $ liftA (filter ('d' `elem`)) (Identity ["hello", "world"]) `shouldBe` Identity ["world"]

monadSpec :: SpecWith ()
monadSpec = do
  it "liftM" $ liftM (filter ('d' `elem`)) (Identity ["hello", "world"]) `shouldBe` Identity ["world"]
  it "forM" $ forM ["hello", "world"] (Identity . fmap toUpper) `shouldBe` Identity ["HELLO", "WORLD"]
  it "when" $ do
    r <- newIORef 0
    when True (writeIORef r 1)
    v <- readIORef r
    v `shouldBe` 1
  it "when" $ do
    r <- newIORef 0
    when False (writeIORef r 1)
    v <- readIORef r
    v `shouldBe` 0
  it "unless" $ do
    r <- newIORef 0
    unless False (writeIORef r 1)
    v <- readIORef r
    v `shouldBe` 1
  it "unless" $ do
    r <- newIORef 0
    unless True (writeIORef r 1)
    v <- readIORef r
    v `shouldBe` 0

exceptionSpec :: SpecWith ()
exceptionSpec = do
  it "bracket" $ do
    bracket (openFile "Data/Test.txt" ReadMode) hClose (\h -> do
      content <- hGetContents h
      content `shouldBe` "Hello World"
      )
  it "withFile" $ do
    withFile "Data/Test.txt" ReadMode (\h -> do
      content <- hGetContents h
      content `shouldBe` "Hello World"
      )

spec::SpecWith ()
spec = do
  describe "preludeSpec" preludeSpec
  describe "maybeSpec" maybeSpec
  describe "eitherSpec" eitherSpec
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
  describe "endoSpec" endoSpec
  describe "applicativeSpec" applicativeSpec
  describe "monadSpec" monadSpec
  describe "exceptionSpec" exceptionSpec

main :: IO ()
main = hspec spec
