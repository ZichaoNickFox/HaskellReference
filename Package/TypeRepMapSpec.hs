{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module Package.TypeRepMapSpec (spec) where

import           Data.TMap
import           Prelude    hiding (lookup)
import           Test.Hspec
import           Util

spec :: SpecWith ()
spec = do
  it "show" $ do
    show (insert True $ one @Int 2021) `shouldBe` "TypeRepMap [Bool, Int]"

  it "insert" $ do
    -- insert :: forall a . Typeable a => a -> TMap -> TMap
    size (insert True $ one @Int 2021) `shouldBe` 2
    lookup @Int (insert @Int 2021 empty) `shouldBe` Just 2021

  it "lookup" $ do
    -- lookup :: forall a . Typeable a => TMap -> Maybe a
    (lookup @Int (insert True $ one @Int 2021)) `shouldBe` (Just 2021)
    (lookup @Bool (insert True $ one @Int 2021)) `shouldBe` (Just True)
    (lookup @String (insert True $ one @Int 2021)) `shouldBe` Nothing

  it "member" $ do
    -- member :: forall a . Typeable a => TMap -> Bool
    (member @Int (insert True $ one @Int 2021)) `shouldBe` True
    (member @Bool (insert True $ one @Int 2021)) `shouldBe` True
    (member @String (insert True $ one @Int 2021)) `shouldBe` False

  it "empty" $ do
    -- empty :: TMap
    size empty `shouldBe` 0

  it "one" $ do
    -- one :: forall a . a -> TMap
    lookup @Int (one @Int 2021) `shouldBe` Just 2021

  it "delete" $ do
    -- delete :: forall a. Typeable a => TMap -> TMap
    (size $ delete @Int $ one @Int 2021) `shouldBe` 0

main :: IO ()
main = hspec spec
