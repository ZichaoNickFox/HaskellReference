{-# LANGUAGE NoFieldSelectors #-}

import           Distribution.TestSuite (Result (Pass))
import           Test.Hspec

newtype Email = Email { raw :: String } deriving (Show, Eq)

spec :: SpecWith ()
spec = return ()
  -- compile error with NoFieldSelectors
  -- it "email" $ raw (Email { raw = "email" }) `shouldBe` "email"

main :: IO ()
main = hspec spec
