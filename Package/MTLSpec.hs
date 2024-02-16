module Package.MTL.MTLSpec where

import           Control.Monad.Except
import           Control.Monad.Trans.Writer
import           Control.Monad.Writer
import           Data.Functor.Identity
import           Data.IORef
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

exceptSpec :: SpecWith ()
exceptSpec = do
  it "runExceptT" $ do
    ioref <- newIORef 0
    let
      lengthValid :: String -> ExceptT String IO ()
      lengthValid input = if length input <= 0 || length input > 5
                          then throwError "error"
                          else liftIO $ modifyIORef ioref (+1)
    runExceptT (lengthValid "hello" >> lengthValid "" >> lengthValid "world")
    v <- readIORef ioref
    v `shouldBe` 1
  it "liftEither" $ do
    r <- runExceptT (liftEither (Right 1 :: Either String Int))
    r `shouldBe` Right 1
  it "liftEither" $ do
    r <- runExceptT (liftEither (Left "err" :: Either String Int))
    r `shouldBe` Left "err"
  it "ExceptT construct" $ do
    r <- runExceptT (ExceptT $ pure (Right 1 :: Either String Int))
    r `shouldBe` Right 1
  it "ExceptT construct" $ do
    r <- runExceptT (ExceptT $ pure (Left "err" :: Either String Int))
    r `shouldBe` Left "err"

spec :: SpecWith ()
spec = do
  describe "writerSpec" writerSpec
  describe "exceptSpec" exceptSpec

main :: IO ()
main = hspec spec
