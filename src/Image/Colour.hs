module Image.Colour
( Colour(..)
, clear
, Point(..)
, V4(..)
) where

import Linear.Affine
import Linear.V4 as Linear
import Linear.Vector

newtype Colour a = Colour (Point V4 a)

clear :: Num a => Colour a
clear = Colour zero

instance Num a => Semigroup (Colour a) where
  Colour p1 <> Colour p2 = Colour (p1 ^+^ p2)

instance Num a => Monoid (Colour a) where
  mempty = clear
  mappend = (<>)
