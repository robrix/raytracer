module Model.Scene where

import Geometry.Ray
import Geometry.Sphere
import Geometry.Vector
import Image.Colour
import Image.Rendering

data Scene = Scene Sphere

trace :: Int -> Scene -> Ray -> Sample
trace 0 _ _ = clear
trace n (Scene sphere) ray@(Ray _ d) = case intersectionsWithSphere ray sphere of
  [] -> clear
  (Intersection _ normal : _) -> Colour (min (abs (_x `dot` normal)) 255) (min (abs (_y `dot` normal)) 255) (min (abs (_z `dot` normal)) 255) (d `dot` normal)
  where _x = Vector 1 0 0
        _y = Vector 0 1 0
        _z = Vector 0 0 1
