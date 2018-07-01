module Main where

import qualified Geometry.Plane as G
import qualified Geometry.Sphere as G
import Linear.Affine
import Linear.V2
import Linear.V3
import Linear.Vector
import Model.Scene
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  renderToFile (V2 800 600) (800 * 600 * 2) path scene
  where scene = Scene
          [ Model (Sphere (G.Sphere (P (V3 0 0 10)) (250 :: Double))) black white
          , Model (Sphere (G.Sphere (P (V3 0 350 0)) 50)) red black
          , Model (Sphere (G.Sphere (P (V3 350 0 0)) 50)) green black
          , Model (Sphere (G.Sphere (P (V3 0 0 (-350))) 50)) blue black
          , top
          , bottom
          , left
          , right
          , back
          , front
          ]
        top = Model (Plane (G.Plane (P (V3 0 500 0)) (-(unit _y)))) black white
        bottom = Model (Plane (G.Plane (P (V3 0 (-500) 0)) (unit _y))) black white
        left = Model (Plane (G.Plane (P (V3 500 0 0)) (-(unit _x)))) black white
        right = Model (Plane (G.Plane (P (V3 (-500) 0 0)) (unit _x))) black white
        back = Model (Plane (G.Plane (P (V3 0 0 500)) (-(unit _z)))) black white
        front = Model (Plane (G.Plane (P (V3 0 0 (-500))) (unit _z))) black white

        black = zero
        white = P (V3 1 1 1)
        red = P (V3 1 0 0)
        green = P (V3 0 1 0)
        blue = P (V3 0 0 1)
