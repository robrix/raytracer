module Image.Rendering where

import Data.ByteString hiding (length)
import Data.ByteString.Internal (c2w)
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
toPPM r = pack header
  where header = fmap c2w $ "P6 " ++ show (getWidth r) ++ " " ++ show (getHeight r) ++ " 255\n"
