{-# LANGUAGE OverloadedStrings #-}

module Package.WaiSpec (spec) where

import           Network.HTTP.Types
import           Network.Wai
-- import Network.Wai.Handler.Warp (run)

import           Test.Hspec

-- https://hackage.haskell.org/package/wai
-- Application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
server :: Application
server _ respond = do
  putStrLn "I did some server"
  respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, client!"

spec :: SpecWith ()
spec = do
  it "sub" $ do
    1 `shouldBe` 1
    -- run 8080 app `shouldBe`

main :: IO ()
main = hspec spec
