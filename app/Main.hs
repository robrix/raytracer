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
rendering = Rendering $ fmap toRow [0..3]
  where toRow i = fmap (toPixel i) [0..3]
        toPixel r b = [ Colour (r / 3) 0 (b / 3) 1 ]
