{-# LANGUAGE DuplicateRecordFields, TypeApplications #-}
module Model.Scene where

import Control.Concurrent.Async
import Control.Monad.Random.Strict
import Control.Monad (replicateM, replicateM_)
import Data.Array
import Data.Array.IO
import Data.Array.Unsafe
import qualified Data.ByteString.Builder as B
import Data.List (foldl1', sortOn)
import Geometry
import Geometry.Path
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
  , emittance   :: {-# UNPACK #-} !(Point V3 a)
  , reflectance :: {-# UNPACK #-} !(Point V3 a)
  }
  deriving (Show)

instance Geometry Model where
  origin (Model geometry _ _) = Geometry.origin geometry
  intersections (Model geometry _ _) = intersections geometry

samplePath :: RealFloat a => Path a -> Sample a
samplePath (Step _ emittance reflectance :< rest)
  = let brdf = reflectance ^/ pi
        incoming = samplePath rest
    in  brdf `seq` incoming `seq` emittance + (brdf * incoming ^/ prob)
  where prob = recip (2 * pi)
samplePath End  = zero

{-# SPECIALIZE samplePath :: Path Float -> Sample Float #-}
{-# SPECIALIZE samplePath :: Path Double -> Sample Double #-}

modelIntersections :: (Epsilon a, RealFloat a) => Model a -> Ray a -> [((a, Intersection a), Model a)]
modelIntersections model = fmap (flip (,) model) . intersections model

{-# SPECIALIZE modelIntersections :: Model Float -> Ray Float -> [((Float, Intersection Float), Model Float)] #-}
{-# SPECIALIZE modelIntersections :: Model Double -> Ray Double -> [((Double, Intersection Double), Model Double)] #-}

cosineHemispheric :: (Random a, RealFloat a) => Distribution (V3 a)
cosineHemispheric = do
  (u1, u2) <- (,) <$> Distribution.unit <*> Distribution.unit
  let r = sqrt u1
      theta = 2 * pi * u2
  pure (V3 (r * cos theta) (sqrt (max 0 (1 - u1))) (r * sin theta))

{-# SPECIALIZE cosineHemispheric :: Distribution (V3 Float) #-}
{-# SPECIALIZE cosineHemispheric :: Distribution (V3 Double) #-}

uniformHemispheric :: (Random a, RealFloat a) => Distribution (V3 a)
uniformHemispheric = do
  (u1, u2) <- (,) <$> Distribution.unit <*> Distribution.unit
  let sinTheta = sqrt (1 - u1 * u1)
      phi = 2 * pi * u2
  pure (V3 (sinTheta * cos phi) u1 (sinTheta * sin phi))

{-# SPECIALIZE uniformHemispheric :: Distribution (V3 Float) #-}
{-# SPECIALIZE uniformHemispheric :: Distribution (V3 Double) #-}

trace :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Scene a -> Ray a -> Distribution (Path a)
trace scene@(Scene models) ray = case models >>= sortOn (fst . fst) . filter ((> 0) . fst . fst) . flip modelIntersections ray of
  [] -> pure End
  ((_, Intersection origin normal), Model _ emittance reflectance) : _ -> do
    v <- cosineHemispheric
    let direction = if normal == Linear.unit _y then
            v
          else
            rotate (Quaternion ((Linear.unit _y `Linear.dot` normal) * pi / 2 + pi) (Linear.unit _y `cross` normal)) v
    (Step (Intersection origin normal) emittance reflectance :<) <$> do
      n <- getRandomR (0, 8 :: Int)
      if n == 0 then
        pure End
      else
        trace scene (Ray origin direction)

{-# SPECIALIZE trace :: Scene Float -> Ray Float -> Distribution (Path Float) #-}
{-# SPECIALIZE trace :: Scene Double -> Ray Double -> Distribution (Path Double) #-}

cast :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Size -> Scene a -> Distribution (Size, Pixel a)
cast (V2 w h) scene = do
  x <- UniformR 0 (pred w)
  y <- UniformR 0 (pred h)
  let ray = Ray (P (V3 (fromIntegral (w `div` 2 - x)) (fromIntegral (h `div` 2 - y)) (-450))) (Linear.unit _z)
  path <- trace scene ray
  let sample = samplePath path
  path `seq` sample `seq` pure (V2 x y, Pixel (Average 1 sample))

{-# SPECIALIZE cast :: Size -> Scene Float -> Distribution (Size, Pixel Float) #-}
{-# SPECIALIZE cast :: Size -> Scene Double -> Distribution (Size, Pixel Double) #-}

render :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Size -> Int -> Scene a -> Distribution (Rendering width height a)
render size n scene = do
  samples <- replicateM n (cast size scene)
  pure (Rendering (accumArray (<>) mempty (0, size) samples))

{-# SPECIALIZE render :: Size -> Int -> Scene Float -> Distribution (Rendering width height Float) #-}
{-# SPECIALIZE render :: Size -> Int -> Scene Double -> Distribution (Rendering width height Double) #-}

renderToFile :: (Conjugate a, Epsilon a, Random a, RealFloat a) => Int -> Size -> Int -> FilePath -> Scene a -> IO ()
renderToFile threads size n path scene = do
  renderings <- replicateConcurrently threads $ do
    array <- newArray @IOArray (0, size) mempty
    replicateM_ (n `div` threads) $ do
      mt <- newPureMT
      (coord, pixel) <- evalRandT (sample (cast size scene)) mt
      pixel' <- (pixel <>) <$> readArray array coord
      pixel' `seq` writeArray array coord pixel'
    Rendering <$> unsafeFreeze array
  withFile path WriteMode (\ handle -> do
    B.hPutBuilder handle (toPNG Depth16 (foldl1' (<>) renderings)))

{-# SPECIALIZE renderToFile :: Int -> Size -> Int -> FilePath -> Scene Double -> IO () #-}
{-# SPECIALIZE renderToFile :: Int -> Size -> Int -> FilePath -> Scene Float -> IO () #-}
