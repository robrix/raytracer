{-# LANGUAGE DuplicateRecordFields, TypeApplications #-}
module Model.Scene where

import Control.Concurrent.Async
import Control.Monad (replicateM, replicateM_)
import Data.Array
import Data.Array.IO
import qualified Data.ByteString.Builder as B
import Data.Foldable (foldr')
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
  { intersection :: {-# UNPACK #-} !(Intersection a)
  , emittance    :: {-# UNPACK #-} !(Point V3 a)
  , reflectance  :: {-# UNPACK #-} !(Point V3 a)
  }

type Path a = [Step a]

samplePath :: RealFloat a => Path a -> Sample a
samplePath = foldr' sampleStep zero
  where sampleStep (Step _ emittance reflectance) incoming =
          let brdf = reflectance ^/ pi
          in  emittance + (brdf * incoming ^/ prob)
        prob = recip (2 * pi)

{-# SPECIALIZE samplePath :: Path Double -> Sample Double #-}

modelIntersections :: (Epsilon a, RealFloat a) => Model a -> Ray a -> [((a, Intersection a), Model a)]
modelIntersections model = fmap (flip (,) model) . intersections model

{-# SPECIALIZE modelIntersections :: Model Double -> Ray Double -> [((Double, Intersection Double), Model Double)] #-}

cosineHemispheric :: (Random a, RealFloat a) => Distribution (V3 a)
cosineHemispheric = do
  (u1, u2) <- (,) <$> Distribution.unit <*> Distribution.unit
  let r = sqrt u1
      theta = 2 * pi * u2
  pure (V3 (r * cos theta) (r * sin theta) (sqrt (max 0 (1 - u1))))

{-# SPECIALIZE cosineHemispheric :: Distribution (V3 Double) #-}

trace :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Int -> Scene a -> Ray a -> Distribution (Path a)
trace 0 _ _ = pure []
trace n scene@(Scene models) ray = case models >>= sortOn (fst . fst) . flip modelIntersections ray of
  [] -> pure []
  ((_, Intersection origin normal), Model _ emittance reflectance) : _ -> do
    v <- cosineHemispheric
    let direction = rotate (Quaternion (Linear.unit _z `Linear.dot` normal) (Linear.unit _z `cross` normal)) v
    (Step (Intersection origin normal) emittance reflectance :) <$> trace (pred n) scene (Ray origin direction)

{-# SPECIALIZE trace :: Int -> Scene Double -> Ray Double -> Distribution (Path Double) #-}

cast :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Size -> Scene a -> Distribution (Size, Pixel a)
cast (V2 w h) scene = do
  x <- UniformR 0 (pred w)
  y <- UniformR 0 (pred h)
  let ray = Ray (P (V3 (fromIntegral (w `div` 2 - x)) (fromIntegral (h `div` 2 - y)) (-450))) (Linear.unit _z)
  path <- trace 8 scene ray
  let sample = samplePath path
  path `seq` sample `seq` pure (V2 x y, Pixel (Average 1 sample))

{-# SPECIALIZE cast :: Size -> Scene Double -> Distribution (Size, Pixel Double) #-}

render :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Size -> Int -> Scene a -> Distribution (Rendering width height a)
render size n scene = do
  samples <- replicateM n (cast size scene)
  pure (Rendering (accumArray (<>) mempty (0, size) samples))

{-# SPECIALIZE render :: Size -> Int -> Scene Double -> Distribution (Rendering width height Double) #-}

renderToFile :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Size -> Int -> FilePath -> Scene a -> IO ()
renderToFile size n path scene = do
  array <- newArray @IOArray (0, size) mempty
  withFile path WriteMode (\ handle -> do
    renderings <- replicateConcurrently threads $ do
      replicateM_ (n `div` 4) $ do
        (coord, pixel') <- sample (cast size scene)
        pixel <- readArray array coord
        writeArray array coord (pixel <> pixel')
      Rendering <$> freeze array
    B.hPutBuilder handle (toPPM Depth16 (foldl1' (<>) renderings)))
  where threads = 4

{-# SPECIALIZE renderToFile :: Size -> Int -> FilePath -> Scene Double -> IO () #-}
