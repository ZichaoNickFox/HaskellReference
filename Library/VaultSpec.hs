module Library.VaultSpec (spec) where

import Data.Vault.Lazy as Vault
import Pattern.StateTSpec hiding (spec)
import Test.Hspec
import Util

vaultSpec :: SpecWith ()
vaultSpec = do
  it "Vault" $ do
    k <- Vault.newKey

    let vault = Vault.insert k "Hello" Vault.empty
    Vault.lookup k vault `shouldBe` Just "Hello"
    l <- Vault.newKey :: IO (Vault.Key Int)
    Vault.lookup l vault `shouldBe` Nothing

    let vault2 = Vault.insert l 1 vault
    Vault.lookup l vault `shouldBe` Nothing
    Vault.lookup l vault2 `shouldBe` Just 1
    Vault.lookup k vault `shouldBe` Just "Hello"

    let vault3 = adjust (++ " World") k vault2
    Vault.lookup k vault3 `shouldBe` Just "Hello World"

    let vault4 = delete k vault3
    Vault.lookup k vault4 `shouldBe` Nothing

    m <- newKey
    let vault5 = insert m 'a' Vault.empty
    let vault6 = union vault4 vault5
    Vault.lookup l vault6 `shouldBe` Just 1
    Vault.lookup m vault6 `shouldBe` Just 'a'

----------------------------------------------------------------------------------------------------

data Test = Test {
  key :: Vault.Key Int
}

vaultInRecordSpec :: SpecWith ()
vaultInRecordSpec = do
  it "Vault In Record" $ do
    m <- Vault.newKey
    let v = insert m 2021 Vault.empty
    let t = Test m
    Vault.lookup (key t) v `shouldBe` Just 2021

spec :: SpecWith ()
spec = do
  vaultSpec  
  vaultInRecordSpec