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

instance Applicative (Distribution num) where
  pure = Pure

  Pure f     <*> a = fmap f a
  (r :>>= k) <*> a = r :>>= ((<*> a) . k)
  f          <*> a = f :>>= (flip fmap a)
