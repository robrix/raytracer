module Geometry.Vector.Spec where

import Geometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary Vector where
  arbitrary = Vector <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "dot" $ do
    it "is zero for orthogonal vectors" $
      (Vector 0 0 1 `dot` Vector 1 0 0) `shouldBe` 0

    prop "is negative for opposed vectors" $
      \ v -> (v `dot` (v * fromScalar (negate 1))) `shouldSatisfy` isNegative

    prop "is commutative" $
      \ a b -> (a `dot` b) `shouldBe` (b `dot` a)

  where isNegative f = f < 0 || isNegativeZero f
