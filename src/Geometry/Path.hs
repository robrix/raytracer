module Geometry.Path where

import Geometry.Ray

newtype Path a = Path [Intersection a]
