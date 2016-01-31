module Geometry.Ray where

import Geometry.Vector

data Ray = Ray { getLocation :: !Vector, getDirection :: !Vector }
