module Image.Colour
( Colour(..)
, clear
, Point(..)
, V4(..)
) where

import Linear.Affine
import Linear.V4 as Linear
import Linear.Vector

newtype Colour = Colour (Point V4 Float)

clear :: Colour
clear = Colour . P $ V4 0 0 0 0

instance Semigroup Colour where
  Colour (P v1) <> Colour (P v2) = Colour (P (v1 ^+^ v2))

instance Monoid Colour where
  mempty = clear
  mappend = (<>)
