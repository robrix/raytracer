module Main where

import Geometry.Sphere
import Linear.Affine
import Linear.V3
import Model.Scene
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  renderToFile path (Scene (Sphere (P (V3 0 0 10)) (250 :: Double)))
