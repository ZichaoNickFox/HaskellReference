{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Package.LensExerciseSpec where

import           Control.Lens
import           Control.Lens.TH
import qualified Data.Text       as T
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

user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

lensISpec :: SpecWith ()
lensISpec =
  it "I.1" $ (user1 ^. name) `shouldBe` "qiao.yifan"
  it "I.2" (user1 ^. metadata . numLogins) `shouldBe` 20
  it "I.3" (user1 & metadata . numLogins .~ 0) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 0
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    )
  it "I.4" (user1 & metadata . associatedIPs %~ ("192.168.0.2" :)) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs =
          [ "192.168.0.2"
          , "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    )
  it "I.5" (metadata . numLogins %~ (+1) $ user1) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 21
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    )

lensIISpec :: SpecWith ()
lensIISpec = do
  it "II.1" $ (user1 & name .~ "qyifan@xingxin.com") `shouldBe`
    (User
      { _name = "qyifan@xingxin.com"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    )
  it "II.2" $ (user1 & metadata .~ UserInfo 17 []) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 17
        , _associatedIPs = []
        }
      })
  it "II.3" (userid .~ -1 $ user1) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = -1
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      })
  it "II.4" (metadata . associatedIPs .~ [ "50.193.0.23" ] $ user1) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs = [ "50.193.0.23" ]
        }
      })
  it "II.5" (user1 ^. metadata . numLogins) `shouldBe` 20

lensIII :: SpecWith ()
lensIII = do
  -- 1. Get the associated IP addresses.
  it "III.1" (user1 ^. metadata . associatedIPs) `shouldBe`
    [ "52.39.193.61", "52.39.193.75" ],
  -- 2. Update the user so that the associated IP addresses are in reverse order.
  it "III.2" (user1 & metadata . associatedIPs %~ reverse) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs =
          [ "52.39.193.75"
          , "52.39.193.61"
          ]
        }
      }),
  -- 3. Update the user so that each word in the name is capitalized.
  it "III.3" (user1 & name %~ T.toUpper) `shouldBe`
    (User
      { _name = "QIAO.YIFAN"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }),
  -- 4. Set the number of logins to 1.
  it "III.4" (user1 & metadata . numLogins .~ 1) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 1
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }),
  -- 5. Remove all associated IP addresses except the first.
  it "III.5" (user1 & metadata . associatedIPs %~ take 1) `shouldBe`
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs = [ "52.39.193.61" ]
        }
      })

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lensExerciseSpec :: SpecWith ()
lensExerciseSpec = do
  lensISpec
  lensIISpec
  lensIIISpec
