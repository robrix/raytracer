module Geometry.Plane where

import Geometry
import Geometry.Ray
import Linear.Affine
import Linear.Metric (dot)
import Linear.V3
import Linear.Vector ((^*))

data Plane a = Plane
  { origin :: !(Point V3 a)
  , normal :: !(V3 a)
  }
  deriving (Eq, Ord, Show)


instance Geometry Plane where
  origin = Geometry.Plane.origin
  intersections (Plane centre normal) (Ray origin direction)
    | denom > 0, t >= 0 = [(t, Intersection intersection (-normal))]
    | otherwise         = []
    where denom = direction `dot` normal
          offset = centre - origin
          t = unP offset `dot` normal / denom
          intersection = origin + P direction ^* t

  {-# SPECIALIZE intersections :: Plane Float -> Ray Float -> [(Float, Intersection Float)] #-}
  {-# SPECIALIZE intersections :: Plane Double -> Ray Double -> [(Double, Intersection Double)] #-}
  {-# INLINABLE intersections #-}
