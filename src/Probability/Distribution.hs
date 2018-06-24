{-# LANGUAGE GADTs #-}
module Probability.Distribution where

data Distribution num a where
  StdRandom :: Distribution num num

  Pure :: a -> Distribution num a
  (:>>=) :: Distribution num b -> (b -> Distribution num a) -> Distribution num a

infixl 1 :>>=

instance Functor (Distribution num) where
  fmap f (Pure a) = Pure (f a)
  fmap f (r :>>= k) = r :>>= fmap f . k
  fmap f a = a :>>= Pure . f
