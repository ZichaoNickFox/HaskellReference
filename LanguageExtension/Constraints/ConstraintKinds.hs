{-# LANGUAGE ConstraintKinds #-}

import           Control.Monad.Reader
import           Data.Has
import           Distribution.Compat.Prelude (undefined)
import           GHC.Conc
import           Test.Hspec

data State
data Auth = Auth
data RegistrationError
data VerificationCode = VerificationCode

type ImMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth :: ImMemory r m => Auth -> m (Either RegistrationError VerificationCode)
addAuth auth = return $ Right VerificationCode

spec :: SpecWith ()
spec = do
  it "ConstraintKinds" $ do
    addAuth Auth `shouldBe` (return (Right VerificationCode) :: IO (Either RegistrationError VerificationCode))

main :: IO ()
main = hspec spec
