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

data Depth = Depth8 | Depth16

toPPM :: RealFrac a => Depth -> Rendering a -> B.Builder
toPPM depth r = header <> encodeRows (pixels r)
  where header = foldMap B.string7 (intersperse " " ["P6", show (renderingWidth r), "", show (renderingHeight r), case depth of { Depth8 -> "255\n" ; Depth16 -> "65535\n" }])
        encodeRows = foldMap encodeRow
        encodeRow = foldMap encodeSamples
        encodeSamples = encodeSample . average
        encodeSample = case depth of
          Depth8 -> foldMap (B.word8 . max 0 . min 255 . round) . view _xyz . (* 255)
          Depth16 -> foldMap (B.word16BE . max 0 . min 65535 . round) . view _xyz . (* 65535)

average :: Fractional a => Pixel a -> Sample a
average p = getAdd (foldMap Add p) ^/ fromIntegral (length p)

newtype Add f a = Add { getAdd :: f a }

instance (Additive f, Num a) => Semigroup (Add f a) where
  Add f1 <> Add f2 = Add (f1 ^+^ f2)

instance (Additive f, Num a) => Monoid (Add f a) where
  mempty = Add zero
  mappend = (<>)
