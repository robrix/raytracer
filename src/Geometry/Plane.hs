module Geometry.Plane where

import Linear.Affine
import Linear.V3

data Plane a = Plane
  { origin :: !(Point V3 a)
  , normal :: !(V3 a)
  }
