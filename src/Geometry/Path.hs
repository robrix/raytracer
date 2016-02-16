module Geometry.Path where

import Geometry.Ray

newtype Path = Path [Intersection]
