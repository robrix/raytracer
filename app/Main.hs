module Main where

import qualified Data.ByteString as ByteString
import Image.Colour
import Image.Rendering
import System.Environment

main :: IO ()
main = do
  [path] <- getArgs
  ByteString.writeFile path (toPPM rendering)

rendering :: Rendering
rendering = Rendering [ [ [ clear ] ] ]
