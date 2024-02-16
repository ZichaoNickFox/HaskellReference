{-# LANGUAGE DuplicateRecordFields #-}

import           Distribution.TestSuite (Result (Pass))
import           Test.Hspec

newtype Email = Email { raw :: String } deriving (Show, Eq)
newtype Password = Password { raw :: String } deriving (Show, Eq)

spec :: SpecWith ()
spec = do
  it "email" $ Email { raw = "email" } `shouldBe` Email { raw = "email" }
  it "passworld" $ Password { raw = "password" } `shouldBe` Password { raw = "password" }

main :: IO ()
main = hspec spec
