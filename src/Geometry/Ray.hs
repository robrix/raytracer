module Geometry.Ray where

import Geometry.Vector
import Geometry.Sphere

data Ray = Ray { getLocation :: !Vector, getDirection :: !Vector }

-- | Compute the set of intersections between a Ray and a Sphere as a list of Vectors in increasing order of distance.
intersectionsWithSphere :: Ray -> Sphere -> [Vector]
intersectionsWithSphere _ _ = []
