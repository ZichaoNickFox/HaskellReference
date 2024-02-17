module Package.STMSpec where

import           GHC.Conc
import           Test.Hspec

tvarSpec :: SpecWith ()
tvarSpec = do
  it "TVar" $ do
    tvar <- newTVarIO (10 :: Int)
    v <- readTVarIO tvar
    v `shouldBe` 10
  it "add 5" $ do
    tvar <- newTVarIO (10 :: Int)
    atomically $ do
      v <- readTVar tvar
      writeTVar tvar (v + 5)
    v <- readTVarIO tvar
    v `shouldBe` 15
  it "add 5" $ do
    tvar <- newTVarIO (10 :: Int)
    v <- readTVarIO tvar
    atomically $ writeTVar tvar (v + 5)
    v <- readTVarIO tvar
    v `shouldBe` 15

spec :: SpecWith ()
spec = do
  describe "tvarSpec" tvarSpec

main :: IO ()
main = hspec spec
