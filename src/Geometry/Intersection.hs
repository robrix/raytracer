module Geometry.Intersection where

import Linear.Affine
import Linear.V3

data Intersection a = Intersection
  { origin   :: {-# UNPACK #-} !(Point V3 a)
  , normal   :: {-# UNPACK #-} !(V3 a)
  }
  deriving (Eq, Ord, Show)
