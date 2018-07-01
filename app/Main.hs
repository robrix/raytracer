module Main where

import qualified Geometry.Sphere as G
import Linear.Affine
import Linear.V2
import Linear.V3
import Model.Scene
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  renderToFile (V2 800 600) (800 * 600 * 2) path scene
  where scene = Scene
          [ Model (Sphere (G.Sphere (P (V3 0 0 10)) (250 :: Double))) (P (V3 0 0 0)) (P (V3 1 1 1))
          , Model (Sphere (G.Sphere (P (V3 0 350 0)) 50)) (P (V3 1 0 0)) (P (V3 0 0 0))
          , Model (Sphere (G.Sphere (P (V3 350 0 0)) 50)) (P (V3 0 1 0)) (P (V3 0 0 0))
          , Model (Sphere (G.Sphere (P (V3 0 0 (-350))) 50)) (P (V3 0 0 1)) (P (V3 0 0 0))
          ]
