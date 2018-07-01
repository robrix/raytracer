module Main where

import Geometry.Sphere
import Linear.Affine
import Linear.V2
import Linear.V3
import Model.Scene
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  renderToFile (V2 800 600) (800 * 600 * 2) path scene
  where scene = Scene (Light (P (V3 0 300 0)) (P (V3 1 1 1)) 20) [Sphere (P (V3 0 0 10)) (250 :: Double)]
