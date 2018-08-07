{-# LANGUAGE GADTs #-}
module Probability.Distribution where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad ((>=>), replicateM)
import Control.Monad.Random.Class (MonadRandom(..))
import Data.Foldable (foldl')
import Data.List (sortOn)
import Prelude hiding (id)
import System.Random (Random(..))

data Distribution a where
  Uniform  :: Random a => Distribution a
  UniformR :: Random a => a -> a -> Distribution a

  Empty :: Distribution a
  Pure :: a -> Distribution a
  (:>>=) :: Distribution b -> (b -> Distribution a) -> Distribution a

infixl 1 :>>=


-- Constructors

unit :: (Num a, Random a) => Distribution a
unit = UniformR 0 1

exponential :: (Floating a, Random a) => Distribution a
exponential = negate (log Uniform)

draw :: [a] -> Distribution a
draw = frequency . map ((,) 1 . pure)

frequency :: [(Int, Distribution a)] -> Distribution a
frequency [] = error "frequency called with empty list"
frequency choices = (UniformR 0 total :: Distribution Int) >>= pick sorted
  where total = sum (fst <$> sorted)
        sorted = reverse (sortOn fst choices)
        pick ((i, a) : rest) n
          | n <= i = a
          | otherwise = pick rest (n - i)
        pick _ _ = error "pick called with empty list"


-- Eliminators

sample :: (Alternative m, MonadRandom m) => Distribution a -> m a
sample Uniform = getRandom
sample (UniformR from to) = getRandomR (from, to)
sample Empty = empty
sample (Pure a) = pure a
sample (a :>>= q) = sample a >>= sample . q

samples :: (Alternative m, MonadRandom m) => Int -> Distribution a -> m [a]
samples n = replicateM n . sample


-- Inspection

histogram :: Real a => [a] -> [a] -> [Int]
histogram []      _       = []
histogram _       []      = []
histogram buckets samples = map fst (foldl' bucketSample (map ((,) 0) buckets) samples)
  where bucketSample accum sample = foldr (\ each rest -> case each of
          (count, from)
            | ((_, to) : _) <- rest
            , from   <= sample
            , sample <= to     -> (succ count, from) : rest
            | otherwise        -> (     count, from) : rest) [] accum

sparkify :: [Int] -> String
sparkify bins
  | null bins = ""
  | otherwise = spark <$> bins
  where sparks = " ▁▂▃▄▅▆▇█"
        maxSpark = pred (length sparks)
        max = maximum bins
        spark n = sparks !! round ((fromIntegral n * ((1.0 :: Double) / fromIntegral max)) * fromIntegral maxSpark)

printHistogram :: Real a => [a] -> Int -> Distribution a -> IO ()
printHistogram buckets n = samples n >=> putStrLn . sparkify . histogram buckets


-- Instances

instance Functor Distribution where
  fmap f (Pure a)   = Pure (f a)
  fmap f (r :>>= q) = r :>>= (q >=> Pure . f)
  fmap f a          = a :>>=        Pure . f

instance Applicative Distribution where
  pure = Pure

  Pure f     <*> a = fmap f a
  (r :>>= q) <*> a = r :>>= (q >=> flip fmap a)
  f          <*> a = f :>>=        flip fmap a

instance Alternative Distribution where
  empty = Empty
  a <|> b = Uniform >>= \ c -> if c then a else b

instance Monad Distribution where
  return = pure

  Pure a     >>= f = f a
  (r :>>= q) >>= f = r :>>= (q >=> f)
  a          >>= f = a :>>=        f

instance MonadRandom Distribution where
  getRandomR (from, to) = UniformR from to
  getRandom = Uniform
  getRandomRs interval = (:) <$> getRandomR interval <*> getRandomRs interval
  getRandoms = (:) <$> getRandom <*> getRandoms

instance Num a => Num (Distribution a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance Fractional a => Fractional (Distribution a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Floating a => Floating (Distribution a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance Bounded a => Bounded (Distribution a) where
  minBound = pure minBound
  maxBound = pure maxBound
