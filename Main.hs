module Main where

import Test.Hspec
import qualified Language.SpecDo

import qualified LanguageExtension.SpecNoImplicitPrelude
import qualified LanguageExtension.SpecRecordWildCards
import qualified LanguageExtension.SpecTypeApplications

import qualified Library.SpecMegaparsec
import qualified Library.SpecPrelude
import qualified Library.SpecText
import qualified Library.SpecTypeRepMap

import qualified Pattern.SpecState
import qualified Pattern.SpecReader
import qualified Pattern.SpecWriter

main :: IO()
main = do
  hspec $ do
    Language.SpecDo.specs
    
    LanguageExtension.SpecNoImplicitPrelude.specs
    LanguageExtension.SpecRecordWildCards.specs
    LanguageExtension.SpecTypeApplications.specs

    Library.SpecMegaparsec.specs
    Library.SpecPrelude.specs
    Library.SpecText.specs
    Library.SpecTypeRepMap.specs

    Pattern.SpecReader.specs
    Pattern.SpecState.specs
    Pattern.SpecWriter.specs