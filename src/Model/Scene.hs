module Model.Scene where

import Control.Parallel.Strategies hiding (dot)
import qualified Data.ByteString as ByteString
import Geometry.Ray
import Geometry.Sphere
import Image.Colour
import Image.Rendering
import Linear.Metric
import Linear.V3

data Scene a = Scene (Sphere a)

trace :: RealFloat a => Int -> Scene a -> Ray a -> Sample a
trace 0 _ _ = clear
trace _ (Scene sphere) ray@(Ray _ d) = case intersectionsWithSphere ray sphere of
  [] -> clear
  (Intersection _ normal : _) -> Colour (P (V4
    (min (abs (_x `dot` normal)) 255)
    (min (abs (_y `dot` normal)) 255)
    (min (abs (_z `dot` normal)) 255)
    (d `dot` normal)))
  where _x = V3 1 0 0
        _y = V3 0 1 0
        _z = V3 0 0 1

render :: (Enum a, RealFloat a) => Scene a -> Rendering a
render scene = Rendering $ withStrategy (parList rpar) $ fmap (fmap (pure . trace 8 scene)) rays
  where width = 800
        height = 600
        row y = [ Ray (P (V3 x y 0)) (V3 0 0 1) | x <- [-width / 2..width / 2] ]
        rays = row <$> [-height / 2..height / 2]


renderToFile :: (Enum a, RealFloat a) => FilePath -> Scene a -> IO ()
renderToFile path = ByteString.writeFile path . toPPM . render
