module Model.Scene where

import Control.Monad.Random.Class (MonadRandom)
import Control.Parallel.Strategies (evalTuple2, parList, r0, rpar, using)
import Data.Array
import qualified Data.ByteString.Builder as B
import Geometry.Ray
import Geometry.Sphere
import Image.Rendering hiding (samples)
import Linear.Affine
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Probability.Distribution hiding (unit)
import System.IO

-- | Sparse 8-tree representation for efficiently storing and querying scenes.
data Octree a
  = Empty
  | Leaf a
  | XYZ (V2 (V2 (V2 (Octree a))))

data Light a = Light
  { lightOrigin :: Point V3 a
  , lightColour :: Point V3 a
  , lightRadius :: a
  }

data Scene a = Scene
  { sceneLights :: Light a
  , sceneModels :: Sphere a
  }

trace :: (Applicative m, RealFloat a) => Int -> Scene a -> Ray a -> m (Sample a)
trace 0 _ _ = pure zero
trace _ (Scene _ sphere) ray@(Ray _ d) = case intersectionsWithSphere ray sphere of
  [] -> pure zero
  Intersection _ normal : _ -> pure (P (V4
    (abs (x `dot` normal))
    (abs (y `dot` normal))
    (abs (z `dot` normal))
    (d `dot` normal)))
  where x = unit _x
        y = unit _y
        z = unit _z

render :: (MonadRandom m, RealFloat a) => Size -> Int -> Scene a -> m (Rendering a)
render size@(V2 w h) n scene = do
  rays <- samples n $ do
    x <- UniformR 0 (pred w)
    y <- UniformR 0 (pred h)
    let ray = Ray (P (V3 (fromIntegral (w `div` 2 - x)) (fromIntegral (h `div` 2 - y)) 0)) (unit _z)
    sample <- trace 8 scene ray
    pure (V2 x y, Pixel [sample])
  pure (Rendering (accumArray (<>) mempty (0, size) (rays `using` parList (evalTuple2 r0 rpar))))

renderToFile :: RealFloat a => Size -> Int -> FilePath -> Scene a -> IO ()
renderToFile size n path scene = withFile path WriteMode (\ handle -> do
  rendering <- render size n scene
  B.hPutBuilder handle (toPPM Depth16 rendering))
