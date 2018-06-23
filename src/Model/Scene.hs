module Model.Scene where

import Control.Parallel.Strategies hiding (dot)
import qualified Data.ByteString.Builder as B
import Geometry.Ray
import Geometry.Sphere
import Image.Rendering
import Linear.Affine
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import System.IO

-- | Sparse 8-tree representation for efficiently storing and querying scenes.
data Octree a
  = Empty
  | Leaf a
  | XYZ (V2 (V2 (V2 (Octree a))))

newtype Scene a = Scene (Sphere a)

trace :: RealFloat a => Int -> Scene a -> Ray a -> Sample a
trace 0 _ _ = zero
trace _ (Scene sphere) ray@(Ray _ d) = case intersectionsWithSphere ray sphere of
  [] -> zero
  Intersection _ normal : _ -> P (V4
    (min (abs (_x `dot` normal)) 255)
    (min (abs (_y `dot` normal)) 255)
    (min (abs (_z `dot` normal)) 255)
    (d `dot` normal))
  where _x = V3 1 0 0
        _y = V3 0 1 0
        _z = V3 0 0 1

data Size = Size { width :: {-# UNPACK #-} !Int, height :: {-# UNPACK #-} !Int }

render :: RealFloat a => Size -> Scene a -> Rendering a
render size scene = Rendering $ withStrategy (parList rpar) $ fmap (fmap (pure . trace 8 scene)) rays
  where rays = [ [ Ray (P (V3 (fromIntegral (width size `div` 2 - x)) (fromIntegral (height size `div` 2 - y)) 0)) (V3 0 0 1)
                 | x <- [0..pred (width  size)] ]
                 | y <- [0..pred (height size)] ]

renderToFile :: RealFloat a => Size -> FilePath -> Scene a -> IO ()
renderToFile size path scene = withFile path WriteMode
  (\ handle -> B.hPutBuilder handle (toPPM16 (render size scene)))
