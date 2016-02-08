module Geometry.Ray where

import Geometry.Vector
import Geometry.Sphere

data Ray = Ray { getLocation :: !Vector, getDirection :: !Vector }

intersectionsWithSphere :: Ray -> Sphere -> [Vector]
intersectionsWithSphere _ _ = []
