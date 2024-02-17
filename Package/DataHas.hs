module Package.DataHas where

import           Data.Has
import           Test.Hspec

data Logger = Logger deriving (Show, Eq)
data DBConnector = DBConnector deriving (Show, Eq)

data Env = Env
  { logger      :: Logger
  , dbConnector :: DBConnector
  }

instance Has Logger Env where
  getter = logger
instance Has DBConnector Env where
  getter = dbConnector

spec :: SpecWith ()
spec = do
  it "hasSpec" $ do
    let env = Env {logger = Logger, dbConnector = DBConnector}
    (getter env :: Logger) `shouldBe` Logger
    (getter env :: DBConnector) `shouldBe` DBConnector

main :: IO ()
main = hspec spec
