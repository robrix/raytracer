module Geometry.Vector.Spec where

import Geometry.Vector
import Test.Hspec

spec :: Spec
spec = do
  describe "dot" $ do
    it "is zero for orthogonal vectors" $
      (Vector 0 0 1 `dot` Vector 1 0 0) `shouldBe` 0
