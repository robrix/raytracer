module Image.Rendering where

import Control.Lens
import qualified Data.ByteString.Builder as B
import Data.List (intersperse)
import Linear.Affine
import Linear.V4
import Linear.Vector

type Sample a = Point V4 a
type Pixel a = [Sample a]

newtype Rendering a = Rendering { pixels :: [[Pixel a]] }

renderingHeight :: Rendering a -> Int
renderingHeight = length . pixels

renderingWidth :: Rendering a -> Int
renderingWidth (Rendering []) = 0
renderingWidth (Rendering (row : _)) = length row

toPPM8 :: RealFrac a => Rendering a -> B.Builder
toPPM8 r = header <> encodeRows (pixels r)
  where header = foldMap B.string7 (intersperse " " ["P6", show (renderingWidth r), "", show (renderingHeight r), "255\n"])
        encodeRows = foldMap encodeRow
        encodeRow = foldMap encodeSamples
        encodeSamples = encodeSample . average
        encodeSample = foldMap (B.word8 . max 0 . min 255 . round) . view _xyz . (* 255)

toPPM16 :: RealFrac a => Rendering a -> B.Builder
toPPM16 r = header <> encodeRows (pixels r)
  where header = foldMap B.string7 (intersperse " " ["P6", show (renderingWidth r), "", show (renderingHeight r), "65535\n"])
        encodeRows = foldMap encodeRow
        encodeRow = foldMap encodeSamples
        encodeSamples = encodeSample . average
        encodeSample = foldMap (B.word16BE . max 0 . min 65535 . round) . view _xyz . (* 65535)

average :: Fractional a => Pixel a -> Sample a
average p = getAdd (foldMap Add p) ^/ fromIntegral (length p)

newtype Add f a = Add { getAdd :: f a }

instance (Additive f, Num a) => Semigroup (Add f a) where
  Add f1 <> Add f2 = Add (f1 ^+^ f2)

instance (Additive f, Num a) => Monoid (Add f a) where
  mempty = Add zero
  mappend = (<>)
