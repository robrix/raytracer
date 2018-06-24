{-# LANGUAGE GADTs #-}
module Probability.Distribution where

import Control.Applicative
import Control.Monad ((>=>), replicateM)
import Control.Monad.Random.Class
import System.Random

data Distribution a where
  StdRandom  :: Random a => Distribution a
  StdRandomR :: Random a => a -> a -> Distribution a
  Let :: a -> (Distribution a -> Distribution a) -> Distribution a

  Pure :: a -> Distribution a
  (:>>=) :: Distribution b -> (b -> Distribution a) -> Distribution a

infixl 1 :>>=


-- Constructors

unit :: (Num a, Random a) => Distribution a
unit = StdRandomR 0 1

listOfN :: Int -> Distribution a -> Distribution [a]
listOfN n element | n > 0 = (:) <$> element <*> listOfN (pred n) element
                  | otherwise = pure []


-- Eliminators

sample :: MonadRandom m => Distribution a -> m a
sample StdRandom = getRandom
sample (StdRandomR from to) = getRandomR (from, to)
sample (Let v f) = sample (f (Pure v))
sample (Pure a) = pure a
sample (a :>>= f) = sample a >>= sample . f

samples :: MonadRandom m => Int -> Distribution a -> m [a]
samples n = replicateM n . sample


-- Instances

instance Functor Distribution where
  fmap f (Pure a)   = Pure (f a)
  fmap f (r :>>= k) = r :>>= fmap f . k
  fmap f a          = a :>>= Pure . f

instance Applicative Distribution where
  pure = Pure

  Pure f     <*> a = fmap f a
  (r :>>= k) <*> a = r :>>= ((<*> a) . k)
  f          <*> a = f :>>= (flip fmap a)

instance Monad Distribution where
  return = pure
  Pure a     >>= f = f a
  (r :>>= k) >>= f = r :>>= (k >=> f)
  a          >>= f = a :>>= f

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
