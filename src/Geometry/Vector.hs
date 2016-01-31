module Geometry.Vector where

import Prelude hiding (zipWith)

data Vector = Vector !Float !Float !Float

zipWith :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
zipWith f (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (f x1 x2) (f y1 y2) (f z1 z2)

magnitude :: Vector -> Float
magnitude (Vector a b c) = sqrt (a * a + b * b + c * c)

instance Num Vector where
  (+) = zipWith (+)
  (*) = zipWith (*)
  (-) = zipWith (-)

  abs (Vector a b c) = Vector (abs a) (abs b) (abs c)

  signum v@(Vector 0 0 0) = v
  signum v@(Vector a b c) = Vector (a / r) (b / r) (c / r) where r = magnitude v

  fromInteger i = Vector (fromInteger i) 0 0
