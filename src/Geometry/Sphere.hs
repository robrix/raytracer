module Geometry.Sphere where

import Control.Applicative ((<**>))
import Geometry
import Geometry.Ray
import Linear.Affine
import Linear.Epsilon
import Linear.Metric (norm, normalize)
import Linear.V3
import Linear.Vector ((^*))

data Sphere a = Sphere
  { origin :: !(Point V3 a)
  , radius :: !a
  }
  deriving (Eq, Ord, Show)

-- | Compute the set of intersections between a Ray and a Sphere as a list of Intersections in increasing order of distance.
intersections :: (Epsilon a, RealFloat a) => Sphere a -> Ray a -> [(a, Intersection a)]
intersections (Sphere centre radius) (Ray origin direction) = if discriminant < 0 then [] else atDistance <$> filter (> 0) ts
  where ts = [-b] <**> [(+), (-)] <*> [sqrt discriminant]
        b = sum (direction * unP translated)
        translated = origin - centre
        discriminant = b ** 2 - norm (unP translated) ** 2 + radius ** 2
        atDistance d = (d, Intersection intersection (normalize (unP (intersection - centre))))
          where intersection = origin + P direction ^* d

instance Geometry Sphere where
  origin = Geometry.Sphere.origin
  intersections = Geometry.Sphere.intersections
