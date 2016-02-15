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

    prop "is the negation of the square of the length of a vector when applied to that vector’s inverse" $
      \ v -> (v `dot` (v * fromScalar (negate 1))) `shouldSatisfy` isWithinEpsilon 1 (negate (magnitude v * magnitude v))

    prop "is commutative" $
      \ a b -> (a `dot` b) `shouldBe` (b `dot` a)

  where isWithinEpsilon _ actual expected | actual == expected = True
        isWithinEpsilon epsilon actual expected = let diff = abs (actual - expected)
                                                      minNormal = fromIntegral (fst (floatRange actual))
                                                      maxValue = fromIntegral (snd (floatRange actual)) in
                                                      if actual == 0 || expected == 0 || diff < minNormal then diff < epsilon * minNormal else diff / min (abs actual + abs expected) maxValue < epsilon
