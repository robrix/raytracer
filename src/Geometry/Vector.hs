module Geometry.Vector where

data Vector = Vector !Float !Float !Float

zipWith :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
zipWith f (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (f x1 x2) (f y1 y2) (f z1 z2)

magnitude :: Vector -> Float
magnitude (Vector a b c) = sqrt (a * a + b * b + c * c)
