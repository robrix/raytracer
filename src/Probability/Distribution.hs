{-# LANGUAGE GADTs #-}
module Probability.Distribution where

data Distribution num a where
  StdRandom :: Distribution num num
