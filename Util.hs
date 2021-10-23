module Util (shouldBeWhat) where

import Test.Hspec

shouldBeWhat :: (Show a) => a -> () -> IO ()
shouldBeWhat a () = do
  print $ show a
