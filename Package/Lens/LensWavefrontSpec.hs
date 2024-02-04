module Package.Lens.LensWavefrontSpec where

import           Codec.Wavefront
import           Codec.Wavefront.IO
import           Control.Lens
import           Data.Vector        hiding (foldr, length, reverse, sum)
import           Test.Hspec

objPath :: String
objPath = "/Users/liuzichao/HGraphics/app/tinyrenderer/res/african_head.obj"
obj :: IO WavefrontOBJ
obj = fromFile objPath >>= either error return

faceVector :: WavefrontOBJ -> Vector (Element Face)
faceVector obj = obj.objFaces

faceElements :: Vector (Element Face) -> [Element Face]
faceElements fv = fv ^.. folded

faces :: [Element Face] -> [Face]
faces = over mapped elValue

temp :: IO ()
temp = do
  print "a"
  -- o <- obj
  -- print $ (faces . faceElements . faceVector) o

-- wavefrontTestList :: SpecWith ()
-- wavefrontTestList = do

wavefrontSpec :: SpecWith ()
wavefrontSpec = it ""
