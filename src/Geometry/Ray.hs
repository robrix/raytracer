module Geometry.Ray where

import Linear.Affine
import Linear.V3

data Ray a = Ray
  { origin    :: {-# UNPACK #-} !(Point V3 a)
  , direction :: {-# UNPACK #-} !(V3 a)
  }
  deriving (Eq, Ord, Show)
