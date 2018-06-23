{-# LANGUAGE DuplicateRecordFields #-}
module Geometry.Ray where

import Data.List (sort)
import Geometry.Sphere
import Linear.Affine
import Linear.V3

data Ray a = Ray
  { origin    :: !(Point V3 a)
  , direction :: !(V3 a)
  }
  deriving (Eq, Ord, Show)

data Intersection a = Intersection
  { origin :: !(Point V3 a)
  , normal :: !(V3 a)
  }
  deriving (Eq, Show)

-- | Compute the set of intersections between a Ray and a Sphere as a list of Intersections in increasing order of distance.
intersectionsWithSphere :: RealFloat a => Ray a -> Sphere a -> [Intersection a]
intersectionsWithSphere (Ray origin direction) (Sphere centre radius) = if discriminant < 0 then [] else atDistance <$> filter (> 0) (sort [ t0, t1 ])
  where t0 = (-b - sqrt discriminant) / 2
        t1 = (-b + sqrt discriminant) / 2
        b = 2 * case P direction * translated of P (V3 dx dy dz) -> dx + dy + dz
        c = case translated of P (V3 tx ty tz) -> tx ** 2 + ty ** 2 + tz ** 2 - radius ** 2
        translated = origin - centre
        discriminant = b ** 2 - 4 * c
        atDistance d = let intersection = origin + P direction * P (V3 d d d) in Intersection intersection (unP (intersection - centre) / V3 radius radius radius)
