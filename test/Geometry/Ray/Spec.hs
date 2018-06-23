module Geometry.Ray.Spec where

import Geometry.Ray
import Geometry.Sphere
import Linear.Affine
import Linear.V3
import Test.Hspec

spec :: Spec
spec = do
  describe "intersectionsWithSphere" $ do
    it "is empty when directed away from the sphere" $
      intersectionsWithSphere (Ray (P (V3 0 0 100)) (V3 0 0 1)) (Sphere (P (V3 0 0 0)) (50 :: Float)) `shouldBe` []

    it "is empty when it misses the sphere" $
      intersectionsWithSphere (Ray (P (V3 100 100 100)) (V3 0 0 (-1))) (Sphere (P (V3 0 0 0)) (50 :: Float)) `shouldBe` []

    it "produces intersections for Ray collisions" $
      intersectionsWithSphere (Ray (P (V3 0 0 100)) (V3 0 0 (-1))) (Sphere (P (V3 0 0 0)) 50)
      `shouldBe`
      [ Intersection (P (V3 0 0   50))  (V3 0 0   1)
      , Intersection (P (V3 0 0 (-50))) (V3 0 0 (-1 :: Float))
      ]
