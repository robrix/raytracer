module Geometry where

import Geometry.Ray
import Linear.Affine
import Linear.Epsilon
import Linear.V3

class Geometry g where
  origin :: g a -> Point V3 a
  intersections :: (Epsilon a, RealFloat a) => g a -> Ray a -> [Intersection a]
