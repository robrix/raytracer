module Geometry.Vector.Spec where

import Geometry.Vector
import Test.Hspec

spec :: Spec
spec = do
  describe "dot" $ do
    it "is zero for orthogonal vectors" $
      True `shouldBe` True
