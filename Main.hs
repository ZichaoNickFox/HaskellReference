module Main where

import Test.Hspec
import qualified Language.DoSpec
import qualified Language.LanguageExtensionSpec

import qualified Library.BaseSpec
import qualified Library.MegaparsecSpec
import qualified Library.TextSpec

import qualified Pattern.StateSpec
import qualified Pattern.ReaderSpec
import qualified Pattern.WriterSpec

main :: IO()
main = do
  hspec $ do
    Language.DoSpec.specs
    Language.LanguageExtensionSpec.specs

    Library.BaseSpec.specs
    Library.MegaparsecSpec.specs
    Library.TextSpec.specs

    Pattern.ReaderSpec.specs
    Pattern.StateSpec.specs
    Pattern.WriterSpec.specs