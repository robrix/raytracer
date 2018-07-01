{-# LANGUAGE DuplicateRecordFields #-}
module Geometry.Ray where

import Control.Applicative ((<**>))
import Data.List (sort)
import Geometry.Sphere
import Linear.Affine
import Linear.Metric (norm)
import Linear.V3
import Linear.Vector ((^*), (^/))

data Ray a = Ray
  { origin    :: !(Point V3 a)
  , direction :: !(V3 a)
  }
  deriving (Eq, Ord, Show)

data Intersection a = Intersection
  { distance :: !a
  , origin   :: !(Point V3 a)
  , normal   :: !(V3 a)
  }
  deriving (Eq, Ord, Show)

-- | Compute the set of intersections between a Ray and a Sphere as a list of Intersections in increasing order of distance.
intersectionsWithSphere :: RealFloat a => Ray a -> Sphere a -> [Intersection a]
intersectionsWithSphere (Ray origin direction) (Sphere centre radius) = if discriminant < 0 then [] else atDistance <$> filter (> 0) (sort ts)
  where ts = [-b] <**> [(+), (-)] <*> [sqrt discriminant]
        b = sum (direction * unP translated)
        translated = origin - centre
        discriminant = b ** 2 - norm (unP translated) ** 2 + radius ** 2
        atDistance d = Intersection d intersection (unP (intersection - centre) ^/ radius)
          where intersection = origin + P direction ^* d
