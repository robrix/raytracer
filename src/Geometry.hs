module Geometry where

import Linear.Affine
import Linear.V3

class Geometry g where
  origin :: g a -> Point V3 a
