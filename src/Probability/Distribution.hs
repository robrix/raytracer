{-# LANGUAGE GADTs #-}
module Probability.Distribution where

data Distribution num a where
  StdRandom :: Distribution num num

  Pure :: a -> Distribution num a
  (:>>=) :: Distribution num b -> (b -> Distribution num a) -> Distribution num a

infixl 1 :>>=
