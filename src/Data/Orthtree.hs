module Data.Orthtree where

data Orthtree i a = Orthtree { getDimensions :: !Int, getOrthants :: !(Orthant i a) }
data Orthant i a = Orthant !(i (a, Orthant i a)) | Nil
