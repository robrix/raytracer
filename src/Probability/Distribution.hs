{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Probability.Distribution where

import Control.Effect
import Control.Effect.Random
import Control.Monad ((>=>), replicateM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (foldl')
import Data.List (sortOn)
import Prelude hiding (id)
import System.Random as R (Random(..), RandomGen(..))

-- Constructors

unit :: (MonadRandom m, Num a, R.Random a) => m a
unit = getRandomR (0, 1)

exponential :: (Floating a, MonadRandom m, R.Random a) => m a
exponential = negate . log <$> getRandom

draw :: MonadRandom m => [a] -> m a
draw = frequency . map ((,) 1 . pure)

frequency :: MonadRandom m => [(Int, m a)] -> m a
frequency [] = error "frequency called with empty list"
frequency choices = getRandomR (0, total) >>= pick sorted
  where total = sum (fst <$> sorted)
        sorted = reverse (sortOn fst choices)
        pick ((i, a) : rest) n
          | n <= i = a
          | otherwise = pick rest (n - i)
        pick _ _ = error "pick called with empty list"


-- Eliminators

samples :: (Carrier sig m, Effect sig, Monad m, RandomGen g) => g -> Int -> Eff (RandomC g m) a -> m [a]
samples g n = evalRandom g . replicateM n


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

printHistogram :: (Carrier sig m, Effect sig, MonadIO m, RandomGen g, Real a) => g -> [a] -> Int -> Eff (RandomC g m) a -> m ()
printHistogram g buckets n = samples g n >=> liftIO . putStrLn . sparkify . histogram buckets
