{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Package.AesonSpec where

-- https://hackage.haskell.org/package/aeson-2.2.1.0/docs/Data-Aeson.html

import           Data.Aeson
import           Data.Aeson.KeyMap
import           Data.Aeson.Types
import qualified Data.Map          as Map
import           Data.Tet
import           GHC.Generics
import           Test.Hspec
import           Text.Parsec       (parse)
import           Util              (shouldBeWhat)

data PersonAutoGen = PersonAutoGen {
  name :: Text,
  age  :: Int
} deriving (Generic, Show, Eq)

instance ToJSON PersonAutoGen where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PersonAutoGen

data PersonByHand = PersonByHand {
  name :: Text,
  age  :: Int
} deriving (Show, Eq)

instance ToJSON PersonByHand where
  toJSON (PersonByHand name age) = object ["name" .= name, "age" .= age]
  toEncoding (PersonByHand name age) = pairs ("name" .= name <> "age" .= age)

instance FromJSON PersonByHand where
  parseJSON = withObject "Person" $ \v -> PersonByHand <$> v .: "name" <*> v .: "age"

usage :: SpecWith ()
usage = do
  describe "encode" $ do
    it "auto gen" $ encode (PersonAutoGen {name="zichao.liu", age=33}) `shouldBe` "{\"name\":\"zichao.liu\",\"age\":33}"
    it "by hand" $ encode (PersonByHand {name="zichao.liu", age=33}) `shouldBe` "{\"name\":\"zichao.liu\",\"age\":33}"

  describe "decode" $ do
    it "auto gen" $ (decode "{\"name\":\"zichao.liu\",\"age\":33}" :: Maybe PersonAutoGen) `shouldBe` Just (PersonAutoGen {name="zichao.liu", age=33})
    it "by hand" $ (decode "{\"name\":\"zichao.liu\",\"age\":33}" :: Maybe PersonByHand) `shouldBe` Just (PersonByHand {name="zichao.liu", age=33})
    it "ast value" $ (decode "{\"name\":\"zichao.liu\",\"age\":33}" :: Maybe Value) `shouldBe` Just (Object (fromList [("age",Number 33), ("name", String "zichao.liu")]))
    it "haskell value" $ (decode "[1, 2, 3]" :: Maybe [Int]) `shouldBe` Just [1, 2, 3]
    it "haskell value" $ (decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map.Map String Int)) `shouldBe` Just (Map.fromList [("bar", 2),("foo",1)])
    it "mixed-typ object" $ do
      let (Just decodeResult) = decode "{\"name\":\"zichao.liu\",\"age\":33}" :: Maybe Object
      decodeResult `shouldBe` fromList [("name",String "zichao.liu"), ("age", Number 33)]

      let parseResult = flip parseMaybe decodeResult $ \o -> do
            age <- o .: "age" :: Parser Int
            name <- o .: "name" :: Parser String
            return $ name ++ " : " ++ show (age `div` 2)
      parseResult `shouldBe` Just "zichao.liu : 16"

spec::SpecWith ()
spec = do
  usage

main :: IO ()
main = hspec spec
