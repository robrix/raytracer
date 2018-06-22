module Image.Rendering where

import Control.Monad
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
toPPM r = pack header <> pack (join (fmap (join . fmap pixelToWords) (getPixels r)))
  where header = fmap c2w $ "P6 " ++ show (getWidth r) ++ " " ++ show (getHeight r) ++ " 255\n"
        pixelToWords p = let Colour (P (V4 r g b _)) = average p in fmap componentToWord [ r, g, b ]
        componentToWord c = max 0 $ min 255 $ round (c * 255)

average :: Pixel -> Colour
average p = case mconcat p of Colour (P (V4 r g b a)) -> Colour (P (V4 (r / l) (g / l) (b / l) (a / l)))
  where l = fromIntegral (length p)
