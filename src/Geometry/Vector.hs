module Geometry.Vector where

import Prelude hiding (zipWith)
import Data.Ratio

data Vector = Vector !Float !Float !Float

fromScalar :: Float -> Vector
fromScalar s = Vector s s s

zero :: Vector
zero = Vector 0 0 0

zipWith :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
zipWith f (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (f x1 x2) (f y1 y2) (f z1 z2)

magnitude :: Vector -> Float
magnitude (Vector a b c) = sqrt (a * a + b * b + c * c)

cross :: Vector -> Vector -> Vector
cross (Vector u1 u2 u3) (Vector v1 v2 v3) = Vector (u2 * v3 - u3 * v2) (u3 * v1 - u1 * v3) (u1 * v2 - u2 * v1)

dot :: Vector -> Vector -> Float
dot a b = case a * b of Vector x y z -> x + y + z

normalize :: Vector -> Vector
normalize v = if d == 0 then zero else v / fromScalar d
  where d = magnitude v

projection :: Vector -> Vector -> Vector
projection a b = fromScalar scalar * unit
  where scalar = a `dot` unit
        unit = normalize b

instance Num Vector where
  (+) = zipWith (+)
  (*) = zipWith (*)
  (-) = zipWith (-)

  abs (Vector a b c) = Vector (abs a) (abs b) (abs c)

  signum v@(Vector 0 0 0) = v
  signum v@(Vector a b c) = Vector (a / r) (b / r) (c / r) where r = magnitude v

  fromInteger i = Vector (fromInteger i) 0 0

instance Fractional Vector where
  Vector x1 y1 z1 / Vector x2 y2 z2 = Vector (x1 / x2) (y1 / y2) (z1 / z2)

  fromRational r = fromInteger (numerator r `div` denominator r)
