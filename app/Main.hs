module Main where

import qualified Data.ByteString as ByteString
import Geometry.Ray
import Geometry.Vector
import Image.Colour
import Image.Rendering
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  ByteString.writeFile path (toPPM rendering)

rendering :: Rendering
rendering = Rendering $ fmap toRow [0..3]
  where toRow i = fmap (toPixel i) [0..3]
        toPixel r b = [ Colour (r / 3) 0 (b / 3) 1 ]

        width = 800
        height = 600
        row y = [ Ray { getLocation = Vector x y 0, getDirection = Vector 0 0 1 } | x <- [-width / 2..width / 2] ]
        rays = do
          y <- [-height / 2..height / 2]
          row y
