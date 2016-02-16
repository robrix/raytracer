module Main where

import Control.Parallel.Strategies hiding (dot)
import qualified Data.ByteString as ByteString
import Geometry.Ray
import Geometry.Sphere
import Geometry.Vector
import Image.Colour
import Image.Rendering
import Model.Scene
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  ByteString.writeFile path . toPPM . render $ Scene Sphere { getCentre = Vector 0 0 10, getRadius = 250 }

render :: Scene -> Rendering
render scene = Rendering $ withStrategy (parList rpar) $ fmap (fmap (pure . trace 8 scene)) rays
  where width = 800
        height = 600
        row y = [ Ray { getLocation = Vector x y 0, getDirection = Vector 0 0 1 } | x <- [-width / 2..width / 2] ]
        rays = row <$> [-height / 2..height / 2]
