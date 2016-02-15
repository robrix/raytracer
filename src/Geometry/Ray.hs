module Geometry.Ray where

import Data.List (sort)
import Geometry.Vector
import Geometry.Sphere

data Ray = Ray { getLocation :: !Vector, getDirection :: !Vector }

data Intersection = Intersection { getGlobalCoordinates :: !Vector, getNormal :: !Vector }
  deriving (Eq, Show)

-- | Compute the set of intersections between a Ray and a Sphere as a list of Intersections in increasing order of distance.
intersectionsWithSphere :: Ray -> Sphere -> [Intersection]
intersectionsWithSphere (Ray origin direction) (Sphere centre radius) = if discriminant < 0 then [] else atDistance <$> filter (> 0) (sort [ t0, t1 ])
  where t0 = -b - sqrt discriminant / 2
        t1 = -b + sqrt discriminant / 2
        b = 2 * case direction * translated of Vector dx dy dz -> dx + dy + dz
        c = case translated of Vector tx ty tz -> tx ** 2 + ty ** 2 + tz ** 2 - radius ** 2
        translated = origin - centre
        discriminant = b ** 2 - 4 * c
        atDistance d = let intersection = direction + origin * fromScalar d in Intersection intersection ((intersection - centre) / fromScalar radius)
