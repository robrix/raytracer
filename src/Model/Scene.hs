{-# LANGUAGE DuplicateRecordFields #-}
module Model.Scene where

import Control.Concurrent.Async
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Random.Strict
import Data.Array
import qualified Data.ByteString.Builder as B
import Data.List (foldl1', sortOn)
import Geometry
import Geometry.Ray
import Image.Rendering hiding (samples)
import Linear.Affine
import Linear.Conjugate
import Linear.Epsilon
import qualified Linear.Metric as Linear
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.Vector as Linear
import Probability.Distribution as Distribution
import System.IO
import System.Random (Random)
import System.Random.Mersenne.Pure64

-- | Sparse 8-tree representation for efficiently storing and querying scenes.
data Octree a
  = Empty
  | Leaf a
  | XYZ a (V2 (V2 (V2 (Octree a))))
  deriving (Eq, Ord, Show)

newtype Scene a = Scene [Model a]
  deriving (Show)

data Model a = Model
  { geometry    :: SomeGeometry a
  , emittance   :: Point V3 a
  , reflectance :: Point V3 a
  }
  deriving (Show)

instance Geometry Model where
  origin (Model geometry _ _) = Geometry.origin geometry
  intersections (Model geometry _ _) = intersections geometry

data Step a = Step
  { intersection :: Intersection a
  , emittance    :: Point V3 a
  , reflectance  :: Point V3 a
  }

type Path a = [Step a]

modelIntersections :: (Epsilon a, RealFloat a) => Model a -> Ray a -> [((a, Intersection a), Model a)]
modelIntersections model = fmap (flip (,) model) . intersections model

cosineHemispheric :: (Random a, RealFloat a) => Distribution (V3 a)
cosineHemispheric = do
  (u1, u2) <- (,) <$> Distribution.unit <*> Distribution.unit
  let r = sqrt u1
      theta = 2 * pi * u2
  pure (V3 (r * cos theta) (r * sin theta) (sqrt (max 0 (1 - u1))))

trace :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Int -> Scene a -> Ray a -> Distribution (Sample a)
trace 0 _ _ = pure zero
trace n scene@(Scene models) ray = case models >>= sortOn (fst . fst) . flip modelIntersections ray of
  [] -> pure zero
  ((_, Intersection origin normal), Model _ emittance reflectance) : _ -> do
    v <- cosineHemispheric
    let direction = rotate (Quaternion (Linear.unit _z `Linear.dot` normal) (Linear.unit _z `cross` normal)) v
        brdf = reflectance ^/ pi
    incoming <- trace (pred n) scene (Ray origin direction)
    pure (emittance + (brdf * incoming ^/ prob))
  where prob = recip (2 * pi)

render :: (Conjugate a, Epsilon a, MonadRandom m, Random a, RealFloat a) => Size -> Int -> Scene a -> m (Rendering width height a)
render size@(V2 w h) n scene = do
  samples <- samples n $ do
    x <- UniformR 0 (pred w)
    y <- UniformR 0 (pred h)
    let ray = Ray (P (V3 (fromIntegral (w `div` 2 - x)) (fromIntegral (h `div` 2 - y)) (-450))) (Linear.unit _z)
    sample <- trace 8 scene ray
    pure (V2 x y, Pixel (Average 1 sample))
  pure (Rendering (accumArray (<>) mempty (0, size) samples))

renderToFile :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Size -> Int -> FilePath -> Scene a -> IO ()
renderToFile size n path scene = do
  mt <- newPureMT
  withFile path WriteMode (\ handle -> do
    renderings <- replicateConcurrently threads (evalRandT (render size (n `div` threads) scene) mt)
    B.hPutBuilder handle (toPPM Depth16 (foldl1' (<>) renderings)))
  where threads = 4
