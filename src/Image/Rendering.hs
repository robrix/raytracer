module Image.Rendering where

import Image.Colour

type Sample = Colour
type Pixel = [Sample]

newtype Rendering = Rendering { getPixels :: [[Pixel]] }
