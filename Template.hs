module Package.Lens.LensSpec where

import           Test.Hspec

lensSpec :: SpecWith ()
lensSpec = do
  it "^.." $ do

spec :: SpecWith ()
spec = do
  describe "lensSpec" lensSpec

main :: IO ()
main = hspec spec
