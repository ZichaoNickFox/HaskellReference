{-# LANGUAGE DeriveGeneric #-}

module Package.AesonSpec where

-- https://hackage.haskell.org/package/aeson-2.2.1.0/docs/Data-Aeson.html

import           Data.Aeson
import           GHC.Generics

spec::SpecWith ()
spec = do

main :: IO ()
main = hspec spec
