module Geometry.Ray.Spec where

import Geometry.Ray
import Geometry.Sphere
import Geometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "intersectionsWithSphere" $ do
    it "is empty when directed away from the sphere" $
      intersectionsWithSphere (Ray (Vector 0 0 100) (Vector 0 0 1)) (Sphere (Vector 0 0 0) 50) `shouldBe` []

    it "is empty when it misses the sphere" $
      intersectionsWithSphere (Ray (Vector 100 100 100) (Vector 0 0 (-1))) (Sphere (Vector 0 0 0) 50) `shouldBe` []

    it "produces intersections for Ray collisions" $
      intersectionsWithSphere (Ray (Vector 0 0 100) (Vector 0 0 (-1))) (Sphere (Vector 0 0 0) 50) `shouldBe` [ Intersection (Vector 0 0 50) (Vector 0 0 1), Intersection (Vector 0 0 (-50)) (Vector 0 0 (-1)) ]
