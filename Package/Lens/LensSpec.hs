module Package.Lens.LensSpec where

import           Control.Lens
import           Control.Lens.Combinators
import           Data.Char
import           Data.Data.Lens
import           Data.Monoid
import           Data.Text                (pack)
import           Data.Text.Lens
import           Test.Hspec
import           Util                     (shouldBeWhat)

-- Github
-- https://github.com/ekmett/lens?tab=readme-ov-file#field-guide

-- Lenses, Folds, and Traversals
-- https://www.youtube.com/watch?v=cefnmjtAolY
videoSpec :: SpecWith ()
videoSpec = do
  it "fmap . fmap" $ (fmap . fmap) (+1) [[1, 2, 3]] `shouldBe` [[2, 3, 4]]
  it "fmap . fmap . fmap" $ (fmap . fmap . fmap) (+1) [[[1, 2, 3]]] `shouldBe` [[[2, 3, 4]]]
  it "fmap . fmap . fmap" $ (fmap . fmap . fmap) (+1) [[[1, 2, 3]], [[4, 5, 6], [7, 8, 9]]] `shouldBe` [[[2, 3, 4]], [[5, 6, 7], [8, 9, 10]]]

  -- fmap law : fmap (f.g) = (fmap g) . (fmap f)
  it "fmap law" $ fmap ((+1) . (+2)) [1, 2, 3] `shouldBe` fmap (+2) (fmap (+1) [1, 2, 3])

-- Control.Lens.Fold
-- https://hackage.haskell.org/package/lens-5.2.3/docs/Control-Lens-Fold.html
-- Can extract and reconstruct values in structure
foldSpec :: SpecWith ()
foldSpec = do
  it "(^..)" $ ([[1, 2], [3]] ^.. id) `shouldBe` [[[1, 2], [3]]]
  it "(^..)" $ ([[1, 2], [3]] ^.. traverse) `shouldBe` [[1, 2], [3]]
  it "(^..)" $ ([[1, 2], [3]] ^.. traverse . traverse) `shouldBe` [1, 2, 3]
  it "(^?)" $ (Left 4 ^? _Left) `shouldBe` Just 4
  it "(^?)" $ (Right 4 ^? _Left :: Maybe Int) `shouldBe` Nothing
  it "(^?)" $ ("world" ^? ix 3) `shouldBe` Just 'l'
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
  it "foldMapOf" $ foldMapOf folded Sum [1, 2, 3] `shouldBe` Sum 6
  it "foldMapOf" $ foldMapOf folded (:[]) [1, 2, 3] `shouldBe` [1, 2, 3]

getterSpec :: SpecWith ()
getterSpec = do
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

  --Control.Lens.Getter
  it "(^.)" $ ("hello", "world") ^. _2 `shouldBe` "world"
  it "(^.)" $ (1, 2) ^. _1 `shouldBe` 1
  it "(^..)" $ ((1, 2, 3, 4, 5) ^.. (_1 <> _3 <> _5)) `shouldBe` [1, 3, 5]

  it "allOf" $ allOf (folded . folded) isLower ["hello", "world"] `shouldBe` True
  it "anyOf" $ anyOf biplate (=="world") ("hello", (), ("hello", ("world", 1::Int))) `shouldBe` True

  it "from to" $ ("hello" ^. from packed . to length) `shouldBe` 5

setterSpec :: SpecWith ()
setterSpec = do
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

-- https://hackage.haskell.org/package/lens-5.2.3/docs/Control-Lens-Combinators.html#v:_Just
combinatorSpec :: SpecWith ()
combinatorSpec = do
  -- traverse :: (a -> f b) -> t a -> f (t b)
  it "traverse" $ traverse Just [1, 2, 3] `shouldBe` Just [1, 2, 3]
  it "traverse" $ traverse ((+1)<$>) [Right 1, Right 2, Right 3] `shouldBe` (Right [2, 3, 4] :: Either Int [Int])
  it "traverse" $ traverse id [Right 1, Left 2, Right 3, Left 4] `shouldBe` Left 2
  it "bimap" $ bimap toUpper (+1) ('z', 33) `shouldBe` ('Z', 34)
  it "bimap" $ bimap toUpper (+1) (Left 'z') `shouldBe` Left 'Z'
  it "bimap" $ bimap toUpper (+1) (Right 33) `shouldBe` Right 34
  it "foldMapBy" $ foldMapBy (+) 0 length ["hello", "world"] `shouldBe` 10
  it "foldBy" $ foldBy (++) "" ["hello", "world"] `shouldBe` "helloworld"
  it "ifolded withIndex" $ [10, 20, 30] ^.. ifolded . withIndex `shouldBe` [(0, 10), (1, 20), (2, 30)]
  let nat :: Prism' Integer Int
      nat = prism toInteger $ \ i -> if i < 0 then Left i else Right (fromInteger i)
  it "prism" $ 5 ^? nat `shouldBe` Just (5 :: Int)
  it "prism" $ (-5) ^? nat `shouldBe` Nothing
  it "prism" $ ((-3, 4) & both . nat *~ 2) `shouldBe` (-3, 8)
  it "prism" $ 5 ^. re nat `shouldBe` 5
  -- TODO: before _Just
  it "_Just" $ over _Just (+1) (Just 2) `shouldBe` Just 3
  it "_Just" $ _Just # 5 `shouldBe` Just 5
  it "_Just" $ 5 ^. re _Just `shouldBe` Just 5
  it "_Just" $ Just 1 ^? _Just `shouldBe` Just 1
  it "_Just" $ Nothing ^? _Just `shouldBe` (Nothing :: Maybe Int)
  it "_Just" $ Just LT ^. _Just `shouldBe` LT
  -- TODO: after _Just
  it "foled" $ Just 3 ^.. folded `shouldBe` [3]
  it "foled" $ Nothing ^.. folded `shouldBe` ([] :: [Int])
  it "foled" $ [(1, 2), (3, 4)] ^.. folded `shouldBe` [(1, 2), (3, 4)]
  it "foled" $ [(1, 2), (3, 4)] ^.. folded.both `shouldBe` [1, 2, 3, 4]
  it "filtered" $ [1, 2, 3] ^.. folded.filtered even `shouldBe` [2]
  it "filtered" $ ["hello", "world", "!"] ^.. folded.filtered (\n -> 'h' `elem` n) `shouldBe` ["hello"]
  it "filtered" $ [(1, 2), (3, 4)] ^.. folded.both.filtered even `shouldBe` [2, 4]
  it "sequenceAOf" $ sequenceAOf both ([1, 2], [3, 4]) `shouldBe` [(1, 3), (1, 4), (2, 3), (2, 4)]

textSpec :: SpecWith ()
textSpec = do
  it "text" $ pack "hello world" ^.. unpacked.folded.filtered (== 'o') `shouldBe` "oo"

spec::SpecWith ()
spec = do
  describe "videoSpec" videoSpec
  describe "fold" foldSpec
  describe "getterSpec" getterSpec
  describe "setterSpec" setterSpec
  describe "combinatorSpec" combinatorSpec
  describe "textSpec" textSpec

main :: IO ()
main = hspec spec
