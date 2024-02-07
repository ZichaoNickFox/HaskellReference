module Util (shouldBeWhat) where

import Debug.Trace (traceShow)
import Test.Hspec

shouldBeWhat :: (Show a) =>  a -> () -> IO ()
shouldBeWhat a () = traceShow a (return ())
