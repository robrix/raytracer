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
