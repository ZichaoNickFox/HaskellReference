{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

import           Distribution.TestSuite (Result (Pass))
import           Test.Hspec

newtype Email = Email { raw :: String } deriving (Show, Eq)
newtype Password = Password { raw :: String } deriving (Show, Eq)

spec :: SpecWith ()
spec = do
  it "email" $ Email { raw = "email" }.raw `shouldBe` "email"
  it "passworld" $ Password { raw = "password" }.raw `shouldBe` "password"

main :: IO ()
main = hspec spec
