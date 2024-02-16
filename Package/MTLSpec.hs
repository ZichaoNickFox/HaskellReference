module Package.MTL.MTLSpec where

import           Control.Monad.Trans.Writer (runWriter)
import           Control.Monad.Writer
import           Data.Functor.Identity
import           Test.Hspec

writerSpec :: SpecWith()
writerSpec = do
  let diary :: Int -> Writer String Int
      diary i = WriterT (Identity (i, show i ++ " day's diary\n"))
  it "runWriterT" $
    runWriterT (diary 1 >> diary 2 >> diary 3)
      `shouldBe` Identity (3, "1 day's diary\n2 day's diary\n3 day's diary\n")
  it "execWriterT" $
    execWriterT (diary 1 >> diary 2 >> diary 3)
      `shouldBe` Identity "1 day's diary\n2 day's diary\n3 day's diary\n"
  it "mapWriterT" $
    mapWriterT id (diary 1 >> diary 2 >> diary 3)
      `shouldBe` WriterT (Identity (3, "1 day's diary\n2 day's diary\n3 day's diary\n"))

spec :: SpecWith ()
spec = do
  describe "writerSpec" writerSpec

main :: IO ()
main = hspec spec
