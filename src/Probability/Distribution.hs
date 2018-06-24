{-# LANGUAGE GADTs #-}
module Probability.Distribution where

import Control.Applicative (liftA2)
import Control.Category ((>>>), id)
import Control.Monad ((>=>), replicateM)
import Control.Monad.Random.Class (MonadRandom(..))
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.TASequence.BinaryTree (BinaryTree, TASequence(..), TAViewL(..))
import Prelude hiding (id)
import System.Random (Random(..))

data Distribution a where
  Uniform  :: Random a => Distribution a
  UniformR :: Random a => a -> a -> Distribution a

  Let :: a -> (Distribution a -> Distribution a) -> Distribution a

  Pure :: a -> Distribution a
  (:>>=) :: Distribution b -> Queue b a -> Distribution a

infixl 1 :>>=

newtype Arrow a b = Arrow { runArrow :: a -> Distribution b }
type Queue = BinaryTree Arrow

apply :: Queue a b -> a -> Distribution b
apply q a = case tviewl q of
  TAEmptyL -> pure a
  h :< t   -> case runArrow h a of
    Pure b     -> apply t b
    h' :>>= q' -> h' :>>= (q' >>> t)
    h'         -> h' :>>=         t


-- Constructors

unit :: (Num a, Random a) => Distribution a
unit = UniformR 0 1

exponential :: (Floating a, Random a) => Distribution a
exponential = negate (log Uniform)

listOf :: Distribution a -> Distribution [a]
listOf element = do
  n <- UniformR 0 10 :: Distribution Int
  listOfN n element

listOfN :: Int -> Distribution a -> Distribution [a]
listOfN n element | n > 0 = (:) <$> element <*> listOfN (pred n) element
                  | otherwise = pure []

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

sample :: MonadRandom m => Distribution a -> m a
sample Uniform = getRandom
sample (UniformR from to) = getRandomR (from, to)
sample (Let v f) = sample (f (Pure v))
sample (Pure a) = pure a
sample (a :>>= q) = sample a >>= sample . apply q

samples :: MonadRandom m => Int -> Distribution a -> m [a]
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
  fmap f (r :>>= q) = r :>>= q  |> Arrow (Pure . f)
  fmap f a          = a :>>= id |> Arrow (Pure . f)

instance Applicative Distribution where
  pure = Pure

  Pure f     <*> a = fmap f a
  (r :>>= q) <*> a = r :>>= q  |> Arrow (flip fmap a)
  f          <*> a = f :>>= id |> Arrow (flip fmap a)

instance Monad Distribution where
  return = pure

  Pure a     >>= f = f a
  (r :>>= q) >>= f = r :>>= q  |> Arrow f
  a          >>= f = a :>>= id |> Arrow f

instance Semigroup a => Semigroup (Distribution a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Distribution a) where
  mempty = pure mempty
  mappend = (<>)

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
