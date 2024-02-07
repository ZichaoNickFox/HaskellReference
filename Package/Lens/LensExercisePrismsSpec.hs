{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Package.Lens.LensExercisePrismsSpec where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Identity (Identity (runIdentity))
import           Data.Aeson.Lens
import           Data.Aeson.QQ
import           Data.Monoid
import qualified Data.Text              as T
import           Test.Hspec

-- https://williamyaoh.com/posts/2019-04-25-lens-exercises.html

user1 = [aesonQQ|
  {
    "name": "qiao.yifan",
    "email": "qyifan@xingxin.com"
  }
|]

user2 = [aesonQQ|
  {
    "name": "ye.xiu",
    "metadata": {
      "num_logins": 27
    }
  }
|]

prismsISpec :: SpecWith ()
prismsISpec = do
  it "^?" $ user1 ^? key "metadata".key "num_logins"._Integer `shouldBe` Nothing
  it "^?" $ user1 ^? key "email"._String `shouldBe` Just "qyifan@xingxin.com"
  it "^?" $ user2 ^? key "email"._String `shouldBe` Nothing
  it "^." $ user2 ^. key "email"._String `shouldBe` ""
  it "^?" $ user2 ^? key "metadata".key "associated_ips"._Array `shouldBe` Nothing
  it "^." $ user2 ^. key "metadata".key "associated_ips"._Array `shouldBe` mempty
  it "^? to" $ user1 ^? key "name"._String.to T.toUpper `shouldBe` Just "QIAO.YIFAN"
  it ".~" $ (user1 & key "name"._String .~ "su.mucheng") `shouldBe`
    [aesonQQ|
      {
        "name": "su.mucheng",
        "email": "qyifan@xingxin.com"
      }
    |]
  it ".~" $ (user2 & key "email"._String .~ "yxiu@xingxin.com") `shouldBe`
    [aesonQQ|
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 27
        }
      }
    |]
  it "%~" $ (user2 & key "name"._String %~ T.reverse) `shouldBe`
    [aesonQQ|
      {
        "name": "uix.ey",
        "metadata": {
          "num_logins": 27
        }
      }
    |]

prismsIISpec :: SpecWith ()
prismsIISpec = do
  it "^?" $ user2 ^? key "metadata".key "num_logins"._Integer `shouldBe` Just 27
  it ".~" $ (user1 & key "metadata".key "num_logins"._Integer .~ 25) `shouldBe`
    [aesonQQ|
      {
        "name": "qiao.yifan",
        "email": "qyifan@xingxin.com"
      }
    |]
  it ".~" $ (user2 & key "metadata".key "num_logins"._Integer %~ (+1)) `shouldBe`
    [aesonQQ|
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 28
        }
      }
    |]
  it "^." $ user1 ^. key "email"._String `shouldBe` "qyifan@xingxin.com"
  it "&" $ (user2 & key "name"._String .~ "50") `shouldBe`
    [aesonQQ|
      {
        "name": "50",
        "metadata": {
          "num_logins": 27
        }
      }
    |]

type Prism s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
-- type Lens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

infixl 8 ^?^?
-- // LINK Package/BaseSpec.hs#First
(^?^?) :: s -> ((a -> Const (First a) b) -> s -> Const (First a) t) -> Maybe a
(^?^?) s l = getFirst $ getConst $ l (Const . First . Just) s

infixr 4 %~%~
(%~%~) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
(%~%~) l f s = runIdentity $ l (Identity . f) s

-- // LINK Package/BaseSpec.hs#Const
infixl 8 ^.^.
(^.^.) :: s -> ((a -> Const a b) -> s -> Const a t) -> a
(^.^.) s l = getConst $ l Const s

_Just_Just :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
_Just_Just g (Just a) = Just <$> g a
_Just_Just g Nothing = pure Nothing

prismsIIISpec :: SpecWith ()
prismsIIISpec = do
  it "^?^?" $ user2 ^?^? key "metadata".key "num_logins"._Integer `shouldBe` Just 27
  it "^?^?" $ user2 ^?^? key "metadata_null".key "num_logins"._Integer `shouldBe` Nothing
  it "_Just" $ Just LT ^. _Just_Just `shouldBe` LT
  it "^.^." $ Just LT ^.^. _Just_Just `shouldBe` LT
  it "%~%~" $ (Just LT & _Just_Just %~%~ show) `shouldBe` Just "LT"

lensExercisePrismsSpec :: SpecWith ()
lensExercisePrismsSpec = do
  describe "prismsISpec" prismsISpec
  describe "prismsIISpec" prismsIISpec
  describe "prismsIIISpec" prismsIIISpec

main :: IO ()
main = hspec lensExercisePrismsSpec
