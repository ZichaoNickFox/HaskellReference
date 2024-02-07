{-# LANGUAGE OverloadedStrings #-}

module Package.Lens.LensAesonSpec where

import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Lens
import           Data.String
import qualified Data.Vector       as Vector
import           Test.Hspec
import           Util (shouldBeWhat)

numbersSpec :: SpecWith ()
numbersSpec = do
  it "_Number" $ ("[1, \"x\"]" :: String) ^? nth 0 . _Number `shouldBe` Just 1
  it "_Number" $ ("[1, \"x\"]" :: String) ^? nth 1 . _Number `shouldBe` Nothing
  it "_Double" $ ("[10.2]" :: String) ^? nth 0 . _Double `shouldBe` Just 10.2
  it "_Integer" $ ("[10]" :: String) ^? nth 0 . _Integer `shouldBe` Just 10
  it "_Integer" $ ("[10.2]" :: String) ^? nth 0 . _Integer `shouldBe` Just 10
  it "_Integer" $ ("42" :: String) ^? _Integer `shouldBe` Just 42
  it "_Integral" $ ("10" :: String) ^? _Integral `shouldBe` Just 10
  it "_Integral" $ ("10.2" :: String) ^? _Integral `shouldBe` Just 10
  it "nonNull" $ ("{\"name\":\"zichao.liu\",\"age\":null}" :: String) ^? key "name".nonNull `shouldBe` Just (String "zichao.liu")
  it "nonNull" $ ("{\"name\":\"zichao.liu\",\"age\":null}" :: String) ^? key "age".nonNull `shouldBe` Nothing

objectAndArraySpec :: SpecWith ()
objectAndArraySpec = do
  it "_Value" $ ("[1, 2, 3]" :: String) ^? _Value `shouldBe` Just (Array (Vector.fromList [Number 1, Number 2, Number 3]))
  it "_String" $ ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) ^? key "name" . _String `shouldBe` Just "zichao.liu"
  it "_String" $ ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) ^? key "male" . _String `shouldBe` Nothing
  it "_Bool" $ ("{\"name\":\"zichao.liu\",\"age\":33, \"male\":true}" :: String) ^? key "male" . _Bool `shouldBe` Just True
  it "_Bool" $ ("{\"name\":\"zichao.liu\",\"age\":33, \"male\":true}" :: String) ^? key "zichao.liu" . _Bool `shouldBe` Nothing
  it "_Bool" $ ("{\"name\":\"zichao.liu\",\"age\":33, \"male\":true, \"null\":null}" :: String) ^? key "null" . _Bool `shouldBe` Nothing
  it "_Null" $ ("{\"name\":\"zichao.liu\",\"age\":33, \"null\":null}" :: String) ^? key "null" . _Null `shouldBe` Just ()
  it "_Null" $ ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) ^? key "male" . _Null `shouldBe` Nothing
  it "_Object" $ ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) ^? _Object `shouldBe` Just (KeyMap.fromList [("age", Number 33.0), ("name", String "zichao.liu")])
  it "_Object" $ ("[1, 2, 3]" :: String) ^? _Object `shouldBe` Nothing
  it "_Array" $ ("[1, 2, 3]" :: String) ^? _Array `shouldBe` Just (Vector.fromList [Number 1, Number 2, Number 3])
  it "key" $ ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) ^? key "name" `shouldBe` Just (String "zichao.liu")
  it "key" $ ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) ^? key "male" `shouldBe` Nothing
  it "atKey .~" $ (("{\"name\":\"zichao.liu\",\"age\":33}" :: String) & atKey "age" .~ Nothing) `shouldBe` "{\"name\":\"zichao.liu\"}"
  it "nth" $ ("[1, 2, 3]" :: String) ^? nth 1 `shouldBe` Just (Number 2)
  it "nth" $ ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) ^? nth 1 `shouldBe` Nothing
  it "nth" $ (("[1, 2, 3]" :: String) & nth 1 .~ Number 20) `shouldBe` "[1,20,3]"
  it "values" $ ("[1, 2, 3]" :: String) ^.. values `shouldBe` [Number 1, Number 2, Number 3]
  it "values _Number" $ (("[1, 2, 3]" :: String) ^.. values. _Number) `shouldBe` [1, 2, 3]
  it "preview" $ preview _Value ("[1, 2, 3]" :: String) `shouldBe` Just (Array (Vector.fromList [Number 1, Number 2, Number 3]))
  it "preview" $ preview _Object ("{\"name\":\"zichao.liu\",\"age\":33}" :: String) `shouldBe` Just (KeyMap.fromList [("age", Number 33.0), ("name", String "zichao.liu")])
  it "preview" $ preview _Array ("[1, 2, 3]" :: String) `shouldBe` Just (Vector.fromList [Number 1, Number 2, Number 3])

spec :: SpecWith ()
spec = do
  describe "numbersSpec" numbersSpec
  describe "objectAndArraySpec" objectAndArraySpec

main :: IO ()
main = hspec spec
