module Image.Colour where

data Colour = Colour !Float !Float !Float !Float

clear :: Colour
clear = Colour 0 0 0 0

instance Monoid Colour where
  mempty = clear
  mappend (Colour r1 g1 b1 a1) (Colour r2 g2 b2 a2) = Colour (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)
