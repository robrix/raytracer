module Geometry.Sphere where

import Geometry.Vector

data Sphere = Sphere { getCentre :: !Vector, getRadius :: !Float }
