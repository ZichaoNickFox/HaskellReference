module Main where

import Test.Hspec
import qualified Language.SpecDo

import qualified LanguageExtension.SpecRecordWildCards

import qualified Library.SpecBase
import qualified Library.SpecMegaparsec
import qualified Library.SpecText
import qualified Library.SpecTypeRepMap

import qualified Pattern.SpecState
import qualified Pattern.SpecReader
import qualified Pattern.SpecWriter

main :: IO()
main = do
  hspec $ do
    Language.SpecDo.specs
    
    LanguageExtension.SpecRecordWildCards.specs

    Library.SpecBase.specs
    Library.SpecMegaparsec.specs
    Library.SpecText.specs
    Library.SpecTypeRepMap.specs

    Pattern.SpecReader.specs
    Pattern.SpecState.specs
    Pattern.SpecWriter.specs