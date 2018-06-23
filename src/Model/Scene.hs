module Model.Scene where

import Geometry.Ray
import Geometry.Sphere
import Image.Colour
import Image.Rendering
import Linear.Metric
import Linear.V3

data Scene a = Scene (Sphere a)

trace :: RealFloat a => Int -> Scene a -> Ray a -> Sample a
trace 0 _ _ = clear
trace n (Scene sphere) ray@(Ray _ d) = case intersectionsWithSphere ray sphere of
  [] -> clear
  (Intersection _ normal : _) ->
    Colour (P (V4 (min (abs (_x `dot` normal)) 255) (min (abs (_y `dot` normal)) 255) (min (abs (_z `dot` normal)) 255) (d `dot` normal)))
  where _x = V3 1 0 0
        _y = V3 0 1 0
        _z = V3 0 0 1
