{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Package.Lens.LensExerciseLensSpec where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Identity (Identity (runIdentity))
import qualified Data.Text              as T
import           Test.Hspec

-- https://williamyaoh.com/posts/2019-04-25-lens-exercises.html

data User = User
  { _name     :: T.Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show, Eq)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [T.Text]
  }
  deriving (Show, Eq)

makeLenses ''User
makeLenses ''UserInfo

users = [aesonQQ|
  {
    "users": [
      {
        "name": "qiao.yifan",
        "email": "qyifan@xingxin.com",
        "metadata": {
          "num_logins": 5
        }
      },
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 27,
          "associated_ips": [
            "52.49.1.233",
            "52.49.1.234"
          ]
        }
      },
      {
        "name": "su.mucheng",
        "email": "smucheng@xingxin.com",
        "metadata": {
          "associated_ips": [
            "51.2.244.193"
          ]
        }
      }
    ]
  }
|]

foldTraversalISpec :: SpecWith ()
foldTraversalISpec = do
  undefined

lensExerciseFoldTraversalSpec :: SpecWith ()
lensExerciseFoldTraversalSpec = do
  describe "foldTraversalISpec" foldTraversalISpec

main :: IO ()
main = hspec lensExerciseLensSpec
