module Geometry.Ray.Spec where

import Geometry.Ray
import Geometry.Sphere
import Geometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "intersectionsWithSphere" $
    it "is empty when directed away from the sphere" $
      intersectionsWithSphere (Ray (Vector 0 0 100) (Vector 0 0 1)) (Sphere (Vector 0 0 0) 50) `shouldBe` []
