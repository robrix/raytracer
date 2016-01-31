module Image.Colour where

data Colour = Colour !Float !Float !Float !Float

clear :: Colour
clear = Colour 0 0 0 0
