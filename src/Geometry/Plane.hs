module Geometry.Plane where

import Geometry.Ray
import Linear.Affine
import Linear.Epsilon
import Linear.Metric (dot)
import Linear.V3
import Linear.Vector ((^*))

data Plane a = Plane
  { origin :: !(Point V3 a)
  , normal :: !(V3 a)
  }

intersections :: (Epsilon a, RealFloat a) => Plane a -> Ray a -> [Intersection a]
intersections (Plane centre normal) (Ray origin direction)
  | denom > 0, t >= 0 = [Intersection t intersection normal]
  | otherwise         = []
  where denom = normal `dot` direction
        offset = centre - origin
        t = unP offset `dot` normal / denom
        intersection = origin + P direction ^* t
