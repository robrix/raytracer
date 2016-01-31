module Image.Rendering where

import Data.ByteString hiding (length)
import Image.Colour

type Sample = Colour
type Pixel = [Sample]

newtype Rendering = Rendering { getPixels :: [[Pixel]] }

getWidth :: Rendering -> Int
getWidth = length . getPixels

getHeight :: Rendering -> Int
getHeight (Rendering []) = 0
getHeight (Rendering (column : _)) = length column

toPPM :: Rendering -> ByteString
toPPM (Rendering pixels) = pack []
