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
    prop "is zero for orthogonal vectors" $
      \ x y -> (Vector x y 0 `dot` Vector (negate y) x 0) `shouldBe` 0

    prop "is the square of the length of a vector when applied to that vector" $
      \ v -> sqrt (v `dot` v) `shouldBe` magnitude v

    prop "is negative for opposed vectors" $
      \ v -> (v `dot` (v * fromScalar (negate 1))) `shouldSatisfy` isNegative

    prop "is commutative" $
      \ a b -> (a `dot` b) `shouldBe` (b `dot` a)

  where isNegative f = f < 0 || isNegativeZero f
