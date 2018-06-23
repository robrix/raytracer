module Main where

import Control.Parallel.Strategies hiding (dot)
import qualified Data.ByteString as ByteString
import Geometry.Ray
import Geometry.Sphere
import Image.Colour
import Image.Rendering
import Linear.Affine
import Linear.V3
import Model.Scene
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  renderToFile path (Scene (Sphere (P (V3 0 0 10)) 250))
