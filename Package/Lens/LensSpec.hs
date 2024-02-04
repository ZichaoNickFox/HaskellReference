module Package.Lens.LensSpec where

import           Control.Lens
import           Data.Char
import           Data.Data.Lens
import           Data.Text.Lens
import           Test.Hspec

lensSpec :: SpecWith ()
lensSpec = do
  -- Lenses, Folds, and Traversals
  -- https://www.youtube.com/watch?v=cefnmjtAolY
  it "fmap . fmap" $ (fmap . fmap) (+1) [[1, 2, 3]] `shouldBe` [[2, 3, 4]]
  it "fmap . fmap . fmap" $ (fmap . fmap . fmap) (+1) [[[1, 2, 3]]] `shouldBe` [[[2, 3, 4]]]
  it "fmap . fmap . fmap" $ (fmap . fmap . fmap) (+1) [[[1, 2, 3]], [[4, 5, 6], [7, 8, 9]]] `shouldBe` [[[2, 3, 4]], [[5, 6, 7], [8, 9, 10]]]

  -- fmap law : fmap (f.g) = (fmap g) . (fmap f)
  it "fmap law" $ fmap ((+1) . (+2)) [1, 2, 3] `shouldBe` fmap (+2) (fmap (+1) [1, 2, 3])

  -- get
  it "get : ^." $ (("hello", "world") ^._2) `shouldBe` "world"
  it "get : ^." $ (("hello", ("world", "!!!")) ^. (_2 . _1)) `shouldBe` "world"
  it "get'" $ (("hello", ("world", "!!!")) ^._2 . _1) `shouldBe` "world"
  it "view" $ view _1 ("hello", "world") `shouldBe` "hello"

  -- length
  it "length" $ ("hello", ("world", "!!!")) ^. _2 ^. _1 . to length `shouldBe` 5

  it "mapped" $ over mapped succ [1, 2, 3] `shouldBe` [2, 3, 4]
  it "mapped.mapped" $ over (mapped . mapped) length [["hello", "world"], ["!!!"]] `shouldBe` [[5, 5], [3]]
  it "mapped._2" $ over (mapped . _2) succ [(1, 2), (3, 4)] `shouldBe` [(1, 3), (3, 5)]

  it "both" $ over both (+1) (1, 2) `shouldBe` (2, 3)
  it "over not work in tuple" $ over mapped (*2) (1, 2, 3) `shouldBe` (1, 2, 6)

  -- Control.Lens.Fold 可以改变结构
  it "(^..)" $ ([[1, 2], [3]] ^.. id) `shouldBe` [[[1, 2], [3]]]
  it "(^..)" $ ([[1, 2], [3]] ^.. traverse) `shouldBe` [[1, 2], [3]]
  it "(^..)" $ ([[1, 2], [3]] ^.. traverse . traverse) `shouldBe` [1, 2, 3]
  it "(^?)" $ (Left 4 ^? _Left) `shouldBe` (Just 4)
  it "(^?)" $ (Right 4 ^? _Left :: Maybe Int) `shouldBe` Nothing
  it "(^?)" $ ("world" ^? ix 3) `shouldBe` (Just 'l')
  it "(^?)" $ ("world" ^? ix 10) `shouldBe` Nothing
  it "(^?!)" $ (Left 4 ^?! _Left) `shouldBe` 4
  it "(^?!)" $ ("world" ^?! ix 3) `shouldBe` 'l'
  it "has" $ has (element 0) [] `shouldBe` False
  it "has" $ has _Left (Left 12) `shouldBe` True
  it "has" $ has _Right (Left 12) `shouldBe` False
  it "hasn't" $ hasn't _Left (Left 12) `shouldBe` False
  it "hasn't" $ hasn't _Left (Right 12) `shouldBe` True
  it "folding" $ ([1, 2, 3, 4] ^.. folding reverse) `shouldBe` [4, 3, 2, 1]
  it "foldring" $ ([1, 2, 3, 4] ^.. foldring foldr) `shouldBe` [1, 2, 3, 4]
  it "folded" $ (Just 3 ^.. folded) `shouldBe` [3]
  it "folded" $ ((Nothing :: Maybe Int) ^.. folded) `shouldBe` []
  it "folded" $ ([(1, 2), (3, 4)] ^.. folded . both) `shouldBe` [1, 2, 3, 4]
  -- TestCase $ assertEqual "foldMapOf" (foldMapOf  sum [1, 2, 3] :: Int) 18

  --Control.Lens.Getter
  it "(^..)" $ ((1, 2, 3, 4, 5) ^.. (_1 <> _3 <> _5)) `shouldBe` [1, 3, 5]

  it "allOf" $ allOf (folded . folded) isLower ["hello", "world"] `shouldBe` True
  it "anyOf" $ anyOf biplate (=="world") ("hello", (), ("hello", ("world", 1::Int))) `shouldBe` True

  it "from to" $ ("hello" ^. from packed . to length) `shouldBe` 5

  -- Control.Lens.Setter 不改变结构，但可能改变类型
  it "over traverse" $ over traverse length ["hello", "world"] `shouldBe` [5, 5]
  it "over _1" $ over _1 length ("hello", "world") `shouldBe` (5, "world")
  it "over traverse._1" $ over (traverse . _1) length [("hello", "hello"), ("world", "world")] `shouldBe` [(5, "hello"), (5, "world")]
  it "over both" $ over both length ("hello", "world") `shouldBe` (5, 5)
  it "over traverse.both" $ over (traverse . both) length [("hello", "hello"), ("world", "world")] `shouldBe` [(5, 5), (5, 5)]
  it "over mapped" $ over mapped length ["hello", "world"] `shouldBe` [5, 5]
  it "set mapped" $ set mapped 1 ["hello", "world"] `shouldBe` [1, 1]
  it "+~" $ (mapped +~ 1) [1, 2, 3] `shouldBe` [2, 3, 4]
  it "*~" $ (both *~ 2 $ (1, 2)) `shouldBe` (2, 4)
  it "over Lifted" $ over lifted Just [1, 2, 3] `shouldBe` [Just 1, Just 2, Just 3]
  it "set" $ set _2 42 ("hello", "world") `shouldBe` ("hello", 42)
  it "set" $ set (_2 . _1) 42 ("hello", ("world", "!!!")) `shouldBe` ("hello", (42, "!!!"))
  it ".~" $ ((1, 2) & _1 .~ "hello" & _2 .~ "world") `shouldBe` ("hello", "world")
  it ".~" $ (_1 . mapped .~ 'a' $ ("hello", "world")) `shouldBe` ("aaaaa", "world")
  it "%~" $ ((1, 2) & _1 %~ show & _2 %~ show) `shouldBe` ("1", "2")
  it "%~" $ (_1 . mapped %~ succ $ ("hello", "world")) `shouldBe` ("ifmmp", "world")
  it "both .~" $ ((1, 2) & both .~ "hello") `shouldBe` ("hello", "hello")
  -- TODO others

  -- Control.Lens.Traversal

spec::SpecWith ()
spec = do
  lensSpec
