module Geometry.Path where

import Geometry.Intersection
import Linear.Affine
import Linear.V3

data Step a = Step
  { intersection :: {-# UNPACK #-} !(Intersection a)
  , emittance    :: {-# UNPACK #-} !(Point V3 a)
  , reflectance  :: {-# UNPACK #-} !(Point V3 a)
  }

data Path a
  = {-# UNPACK #-} !(Step a) :< !(Path a)
  | End
