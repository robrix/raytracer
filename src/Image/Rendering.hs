module Image.Rendering where

import Data.ByteString hiding (length)
import Data.ByteString.Internal (c2w)
import Image.Colour

type Sample = Colour
type Pixel = [Sample]

newtype Rendering = Rendering { getPixels :: [[Pixel]] }

getHeight :: Rendering -> Int
getHeight = length . getPixels

getWidth :: Rendering -> Int
getWidth (Rendering []) = 0
getWidth (Rendering (row : _)) = length row

toPPM :: Rendering -> ByteString
toPPM r = pack header
  where header = fmap c2w $ "P6 " ++ show (getWidth r) ++ " " ++ show (getHeight r) ++ " 255\n"
