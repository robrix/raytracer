module Geometry.Sphere where

import Linear.Affine
import Linear.V3

data Sphere a = Sphere
  { sphereCentre :: !(Point V3 a)
  , sphereRadius :: !a
  }
