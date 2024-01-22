{-# LANGUAGE DeriveDataTypeable #-}
module Package.BaseSpec where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.IORef
import           Data.Typeable
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

----------------------------------------------------------------------------------------------------

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

spec::SpecWith ()
spec = do
  preludeSpec
  dataEitherSpec
  dataTupleSpec
  ioRefSpec
  systemEnvironmentSpec
  typeableSpec
  -- controlMonadSpec

main :: IO ()
main = hspec spec
