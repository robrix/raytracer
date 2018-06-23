module Image.Rendering where

import Control.Monad
import Data.ByteString hiding (length)
import Data.ByteString.Internal (c2w)
import Image.Colour
import Linear.Vector

type Sample a = Colour a
type Pixel a = [Sample a]

newtype Rendering a = Rendering { getPixels :: [[Pixel a]] }

getHeight :: Rendering a -> Int
getHeight = length . getPixels

getWidth :: Rendering a -> Int
getWidth (Rendering []) = 0
getWidth (Rendering (row : _)) = length row

toPPM :: RealFrac a => Rendering a -> ByteString
toPPM r = pack header <> pack (join (fmap (join . fmap pixelToWords) (getPixels r)))
  where header = fmap c2w $ "P6 " ++ show (getWidth r) ++ " " ++ show (getHeight r) ++ " 255\n"
        pixelToWords p = let Colour (P (V4 r g b _)) = average p in fmap componentToWord [ r, g, b ]
        componentToWord c = max 0 $ min 255 $ round (c * 255)

average :: Fractional a => Pixel a -> Colour a
average p = case mconcat p of Colour (P (V4 r g b a)) -> Colour (P (V4 (r / l) (g / l) (b / l) (a / l)))
  where l = fromIntegral (length p)

newtype Add f a = Add { getAdd :: f a }

instance (Additive f, Num a) => Semigroup (Add f a) where
  Add f1 <> Add f2 = Add (f1 ^+^ f2)

instance (Additive f, Num a) => Monoid (Add f a) where
  mempty = Add zero
  mappend = (<>)
