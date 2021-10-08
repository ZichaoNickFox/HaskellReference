module Main where

import Test.Hspec
import qualified BaseSpec
import qualified MegaparsecSpec
import qualified TextSpec
import qualified DoSpec
import qualified StateSpec
import qualified ReaderSpec

main :: IO()
main = do
  hspec $ do
    MegaparsecSpec.specs
    BaseSpec.specs
    TextSpec.specs
    DoSpec.specs
    StateSpec.specs
    ReaderSpec.specs