module Model.Scene where

import Control.Parallel.Strategies (evalTuple2, parList, r0, rpar, using)
import Data.Array
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
    (abs (x `dot` normal))
    (abs (y `dot` normal))
    (abs (z `dot` normal))
    (d `dot` normal))
  where x = unit _x
        y = unit _y
        z = unit _z

render :: RealFloat a => Size -> Scene a -> Rendering a
render size@(V2 w h) scene = Rendering (array (0, size) (rays `using` parList (evalTuple2 r0 rpar)))
  where rays = [ (V2 x y, Pixel [trace 8 scene (Ray (P (V3 (fromIntegral (w `div` 2 - x)) (fromIntegral (h `div` 2 - y)) 0)) (unit _z))])
               | x <- [0..pred w]
               , y <- [0..pred h] ]

renderToFile :: RealFloat a => Size -> FilePath -> Scene a -> IO ()
renderToFile size path scene = withFile path WriteMode
  (\ handle -> B.hPutBuilder handle (toPPM Depth16 (render size scene)))
