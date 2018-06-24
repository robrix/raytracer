{-# LANGUAGE GADTs #-}
module Probability.Distribution where

import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.Random.Class
import System.Random

data Distribution num a where
  StdRandom :: Distribution num num
  Let :: a -> (Distribution num a -> Distribution num a) -> Distribution num a

  Pure :: a -> Distribution num a
  (:>>=) :: Distribution num b -> (b -> Distribution num a) -> Distribution num a

infixl 1 :>>=

sample :: (MonadRandom m, Random num) => Distribution num a -> m a
sample StdRandom = getRandom
sample (Let v f) = sample (f (Pure v))
sample (Pure a) = pure a
sample (a :>>= f) = sample a >>= sample . f

samples :: (MonadRandom m, Random num) => Int -> Distribution num a -> m [a]
samples n = sequenceA . replicate n . sample


instance Functor (Distribution num) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (r :>>= k) = r :>>= fmap f . k
  fmap f a          = a :>>= Pure . f

instance Applicative (Distribution num) where
  pure = Pure

  Pure f     <*> a = fmap f a
  (r :>>= k) <*> a = r :>>= ((<*> a) . k)
  f          <*> a = f :>>= (flip fmap a)

instance Monad (Distribution num) where
  return = pure
  Pure a     >>= f = f a
  (r :>>= k) >>= f = r :>>= (k >=> f)
  a          >>= f = a :>>= f

instance Semigroup a => Semigroup (Distribution num a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Distribution num a) where
  mempty = pure mempty
  mappend = (<>)

instance Num a => Num (Distribution num a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance Fractional a => Fractional (Distribution num a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Floating a => Floating (Distribution num a) where
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

instance Bounded a => Bounded (Distribution num a) where
  minBound = pure minBound
  maxBound = pure maxBound
