{-# LANGUAGE DataKinds, DeriveFunctor, GADTs, GeneralizedNewtypeDeriving, KindSignatures #-}
module Image.Rendering where

import Control.Lens
import Data.Array
import qualified Data.ByteString.Builder as B
import Data.List (intersperse)
import GHC.TypeLits
import Linear.Affine
import Linear.V2
import Linear.V3

type Sample a = Point V3 a
newtype Pixel a = Pixel { samples :: Average (Sample a) }
  deriving (Eq, Monoid, Ord, Semigroup, Show)

type Size = V2 Int

rowMajor :: Size -> (Int -> Int -> a) -> [a]
rowMajor (V2 w h) f = [ f x y | y <- [0..pred h], x <- [0..pred w] ]

newtype Rendering (width :: Nat) (height :: Nat) a where
  Rendering :: { pixels :: Array Size (Pixel a) } -> Rendering width height a

instance Num a => Semigroup (Rendering width height a) where
  Rendering as <> Rendering bs = Rendering (accumArray (<>) mempty (bounds as) (assocs as <> assocs bs))

renderingSize :: Rendering width height a -> Size
renderingSize = snd . bounds . pixels

data Depth = Depth8 | Depth16

toPPM :: RealFrac a => Depth -> Rendering width height a -> B.Builder
toPPM depth r = header <> encodePixels (pixels r)
  where header = foldMap B.string7 (intersperse " " ["P6", show w, "", show h, case depth of { Depth8 -> "255\n" ; Depth16 -> "65535\n" }])
        encodePixels pixels = mconcat (rowMajor size (\ x y -> encodePixel (pixels ! V2 x y)))
        encodePixel = encodeSample . getAverage . samples
        encodeSample = case depth of
          Depth8 -> foldMap (B.word8 . max 0 . min 255 . round) . view _xyz . (* 255)
          Depth16 -> foldMap (B.word16BE . max 0 . min 65535 . round) . view _xyz . (* 65535)
        size@(V2 w h) = renderingSize r


data Average a = Average
  { averageCount   :: {-# UNPACK #-} !Int
  , averageSamples :: !a
  }
  deriving (Eq, Functor, Ord, Show)

getAverage :: Fractional a => Average a -> a
getAverage (Average count samples)
  | count == 0 = samples
  | otherwise  = samples / fromIntegral count

instance Num a => Semigroup (Average a) where
  Average c1 v1 <> Average c2 v2 = Average (c1 + c2) (v1 + v2)

instance Num a => Monoid (Average a) where
  mempty = Average 0 0
  mappend = (<>)
