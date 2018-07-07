{-# LANGUAGE GADTs #-}
module Geometry where

import Geometry.Ray (Ray, Intersection)
import Linear.Affine (Point)
import Linear.Epsilon
import Linear.V3

class Geometry g where
  origin :: g a -> Point V3 a
  intersections :: (Epsilon a, RealFloat a) => g a -> Ray a -> [Intersection a]

data SomeGeometry a where
  SomeGeometry :: Geometry g => g a -> SomeGeometry a

instance Geometry SomeGeometry where
  origin (SomeGeometry g) = origin g
  intersections (SomeGeometry g) = intersections g
