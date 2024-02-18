{-# LANGUAGE TemplateHaskell #-}

module Package.KatipSpec where

import           Katip
import           Test.Hspec

runKatip :: IO ()
runKatip = withKatip $ \le -> runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

spec :: SpecWith ()
spec = do


main :: IO ()
main = hspec spec
