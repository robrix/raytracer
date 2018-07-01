module Model.Scene where

import Control.Monad.Random.Class (MonadRandom)
import Control.Parallel.Strategies (evalTuple2, parList, r0, rpar, using)
import Data.Array
import qualified Data.ByteString.Builder as B
import Data.List (sortOn)
import Geometry.Ray
import Geometry.Sphere
import Image.Rendering hiding (samples)
import Linear.Affine
import Linear.Metric (dot)
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Probability.Distribution hiding (unit)
import System.IO
import System.Random

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
  , sceneModels :: [Model a]
  }

data Model a = Model
  { modelGeometry    :: Sphere a
  , modelReflectance :: Point V4 a
  , modelEmittance   :: Point V4 a
  }

modelIntersections :: RealFloat a => Ray a -> Model a -> [(Intersection a, Model a)]
modelIntersections ray model@(Model sphere _ _) = (,) <$> intersectionsWithSphere ray sphere <*> [model]

trace :: (Random a, RealFloat a) => Int -> Scene a -> Ray a -> Distribution (Sample a)
trace 0 _ _ = pure zero
trace n scene@(Scene _ spheres) ray = case spheres >>= sortOn (distance . fst) . modelIntersections ray of
  [] -> pure zero
  (Intersection _ origin normal, Model _ emittance reflectance) : _ -> do
    direction <- V3 <$> Uniform <*> Uniform <*> Uniform
    let cosTheta = direction `dot` normal
    incoming <- trace (pred n) scene (Ray origin (if cosTheta >= 0 then direction else -direction))
    pure (emittance + (reflectance ^/ pi * incoming ^* abs cosTheta ^/ prob))
  where prob = recip (2 * pi)

render :: (MonadRandom m, Random a, RealFloat a) => Size -> Int -> Scene a -> m (Rendering a)
render size@(V2 w h) n scene = do
  rays <- samples n $ do
    x <- UniformR 0 (pred w)
    y <- UniformR 0 (pred h)
    let ray = Ray (P (V3 (fromIntegral (w `div` 2 - x)) (fromIntegral (h `div` 2 - y)) 0)) (unit _z)
    sample <- trace 8 scene ray
    pure (V2 x y, Pixel [sample])
  pure (Rendering (accumArray (<>) mempty (0, size) (rays `using` parList (evalTuple2 r0 rpar))))

renderToFile :: (Random a, RealFloat a) => Size -> Int -> FilePath -> Scene a -> IO ()
renderToFile size n path scene = withFile path WriteMode (\ handle -> do
  rendering <- render size n scene
  B.hPutBuilder handle (toPPM Depth16 rendering))
