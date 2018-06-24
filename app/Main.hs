module Main where

import Geometry.Sphere
import Image.Rendering
import Linear.Affine
import Linear.V2
import Linear.V3
import Model.Scene
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  renderToFile (V2 800 600) path (Scene (Sphere (P (V3 0 0 10)) (250 :: Double)))
