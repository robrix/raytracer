{-# LANGUAGE DuplicateRecordFields #-}
module Geometry.Ray where

import Linear.Affine
import Linear.V3

data Ray a = Ray
  { origin    :: !(Point V3 a)
  , direction :: !(V3 a)
  }
  deriving (Eq, Ord, Show)

data Intersection a = Intersection
  { origin   :: !(Point V3 a)
  , normal   :: !(V3 a)
  }
  deriving (Eq, Ord, Show)
