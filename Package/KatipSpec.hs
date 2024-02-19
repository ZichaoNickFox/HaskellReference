{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Package.KatipSpec where

import           Control.Exception
import           Data.Text
import           Katip
import           System.IO
import           Test.Hspec

-- CMap
-- https://cmapcloud.ihmc.us/cmaps/myCmaps.html
-- Home -> Haskell -> Katip

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Log in ns1"
  katipAddNamespace "ns2" $ do
    $(logTM) WarningS "Log in ns2"
    katipAddNamespace "ns3" $
      katipAddContext (sl "userId" True) $ do
        $(logTM) InfoS "Log in ns2.ns3 with userId context"
        katipAddContext (sl "country" (pack "China")) $
          $(logTM) InfoS "Log in ns2.ns3 with userId and country context"

runKatip :: IO ()
runKatip = withKatip $ \le -> runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

spec :: SpecWith ()
spec = do
  it "runKatip" $ runKatip

main :: IO ()
main = hspec spec
