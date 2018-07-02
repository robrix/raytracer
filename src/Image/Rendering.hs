{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Image.Rendering where

import Control.Lens
import Data.Array
import qualified Data.ByteString.Builder as B
import Data.List (intersperse)
import Linear.Affine
import Linear.V2
import Linear.V3
import Linear.Vector

type Sample a = Point V3 a
newtype Pixel a = Pixel { samples :: [Sample a] }
  deriving (Eq, Monoid, Ord, Semigroup, Show)

type Size = V2 Int

rowMajor :: Size -> (Int -> Int -> a) -> [a]
rowMajor (V2 w h) f = [ f x y | y <- [0..pred h], x <- [0..pred w] ]

newtype Rendering a = Rendering { pixels :: Array Size (Pixel a) }

renderingSize :: Rendering a -> Size
renderingSize = snd . bounds . pixels

data Depth = Depth8 | Depth16

toPPM :: RealFrac a => Depth -> Rendering a -> B.Builder
toPPM depth r = header <> encodePixels (pixels r)
  where header = foldMap B.string7 (intersperse " " ["P6", show w, "", show h, case depth of { Depth8 -> "255\n" ; Depth16 -> "65535\n" }])
        encodePixels pixels = mconcat (rowMajor size (\ x y -> encodePixel (pixels ! V2 x y)))
        encodePixel = encodeSample . average
        encodeSample = case depth of
          Depth8 -> foldMap (B.word8 . max 0 . min 255 . round) . view _xyz . (* 255)
          Depth16 -> foldMap (B.word16BE . max 0 . min 65535 . round) . view _xyz . (* 65535)
        size@(V2 w h) = renderingSize r

average :: Fractional a => Pixel a -> Sample a
average (Pixel p) = getAdd (foldMap Add p) ^/ fromIntegral (length p)

newtype Add f a = Add { getAdd :: f a }

instance (Additive f, Num a) => Semigroup (Add f a) where
  Add f1 <> Add f2 = Add (f1 ^+^ f2)

instance (Additive f, Num a) => Monoid (Add f a) where
  mempty = Add zero
  mappend = (<>)


data Average a = Average
  { averageCount   :: {-# UNPACK #-} !Int
  , averageSamples :: a
  }
