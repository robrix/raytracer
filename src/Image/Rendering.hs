module Image.Rendering where

import Control.Monad
import Data.ByteString hiding (length, map)
import Data.ByteString.Internal (c2w)
import Linear.Affine
import Linear.V4
import Linear.Vector

type Sample a = Point V4 a
type Pixel a = [Sample a]

newtype Rendering a = Rendering { pixels :: [[Pixel a]] }

getHeight :: Rendering a -> Int
getHeight = length . pixels

getWidth :: Rendering a -> Int
getWidth (Rendering []) = 0
getWidth (Rendering (row : _)) = length row

toPPM :: RealFrac a => Rendering a -> ByteString
toPPM r = pack header <> pack (join (fmap (join . fmap pixelToWords) (pixels r)))
  where header = c2w <$> "P6 " ++ show (getWidth r) ++ " " ++ show (getHeight r) ++ " 255\n"
        pixelToWords p = let P (V4 r g b _) = average p in map componentToWord [ r, g, b ]
        componentToWord c = max 0 $ min 255 $ round (c * 255)

average :: Fractional a => Pixel a -> Sample a
average p = getAdd (foldMap Add p) ^/ fromIntegral (length p)

newtype Add f a = Add { getAdd :: f a }

instance (Additive f, Num a) => Semigroup (Add f a) where
  Add f1 <> Add f2 = Add (f1 ^+^ f2)

instance (Additive f, Num a) => Monoid (Add f a) where
  mempty = Add zero
  mappend = (<>)
