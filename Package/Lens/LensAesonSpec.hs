
module Package.Lens.LensAesonSpec where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.String
import           Test.Hspec

numbersSpec :: SpecWith ()
numbersSpec = do
  it "_Number" $ "[1, \"x\"]" ^? nth 0 . _Number `shouldBe` Just 1
  it "_Number" $ "[1, \"x\"]" ^? nth 1 . _Number `shouldBe` Nothing
  it "_Double" $ "[10.2]" ^? nth 0 . _Double `shouldBe` Just 10.2
  it "_Integer" $ "[10]" ^? nth 0 . _Integer `shouldBe` Just 10
  it "_Integer" $ "[10.2]" ^? nth 0 . _Integer `shouldBe` Just 10
  it "_Integer" $ "42" ^? _Integer `shouldBe` Just 42
  it "_Integral" $ "10" ^? _Integral `shouldBe` Just 10
  it "_Integral" $ "10.2" ^? _Integral `shouldBe` Just 10
  it "nonNull" $ "{\"name\":\"zichao.liu\",\"age\":null}" ^? key (fromString "name").nonNull `shouldBe` Just (String (fromString "zichao.liu"))
  it "nonNull" $ "{\"name\":\"zichao.liu\",\"age\":null}" ^? key (fromString "age").nonNull `shouldBe` Nothing

spec :: SpecWith ()
spec = do
  numbersSpec

main :: IO ()
main = hspec spec
