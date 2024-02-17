module Package.DataDefault where

import           Data.Default
import           Data.Map
import           Data.Set
import           Data.Text
import           Test.Hspec

newtype UserId = UserId Int deriving (Show, Eq)

data Auth = Auth {
  authEmail    :: Email,
  authPassword :: Password
} deriving (Show, Eq)

newtype Email = Email { rawEmail :: Text } deriving (Show, Eq, Ord)
newtype Password = Password { rawPassword :: Text } deriving (Show, Eq)
type VerificationCode = Text
type SessionId = Text

data State = State
  { stateAuths            :: [(UserId, Auth)]
  , stateUnvarifiedEmails :: Map VerificationCode Email
  , stateVerifiedEmails   :: Set Email
  , stateUserIdCounter    :: Int
  , stateNotifications    :: Map Email VerificationCode
  , stateSessions         :: Map SessionId UserId
  } deriving (Show, Eq)

instance Default State where
  def = State
    { stateAuths = def :: [(UserId, Auth)]
      , stateUnvarifiedEmails = def :: Map VerificationCode Email
      , stateVerifiedEmails = def :: Set Email
      , stateUserIdCounter = def :: Int
      , stateNotifications = def :: Map Email VerificationCode
      , stateSessions = def :: Map SessionId UserId
    }

spec :: SpecWith ()
spec = do
  it "def" $ (def :: State) `shouldBe`
    State
    { stateAuths = mempty
    , stateUnvarifiedEmails = mempty
    , stateVerifiedEmails = mempty
    , stateUserIdCounter = 0
    , stateNotifications = mempty
    , stateSessions = mempty
    }

main :: IO ()
main = hspec spec
