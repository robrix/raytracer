module Geometry.Vector.Spec where

import Geometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary Vector where
  arbitrary = Vector <$> arbitraryIntegral <*> arbitraryIntegral <*> arbitraryIntegral
    where arbitraryIntegral = fromIntegral <$> (arbitrary :: Gen Int)

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

  describe "normalize" $ do
    prop "reduces to unit magnitude" $ forAll (arbitrary `suchThat` \ (a, b, c) -> a + b + c /= 0) $
      \ (a, b, c) -> magnitude (normalize (Vector a b c)) `shouldSatisfy` isWithinEpsilon 0.1 1

    it "is identity on the zero vector" $
      let zero = Vector 0 0 0 in normalize zero `shouldBe` zero

  describe "+" $ do
    prop "is associative" $
      \ a b c -> a + (b + c) `shouldBe` (a + b :: Vector) + c

    prop "is commutative" $
      \ a b -> a + b `shouldBe` b + (a :: Vector)

  describe "*" $ do
    prop "is associative" $
      \ a b c -> a * (b * c) `shouldBe` (a * b :: Vector) * c

    prop "is commutative" $
      \ a b -> a * b `shouldBe` b * (a :: Vector)

  where isWithinEpsilon _ actual expected | actual == expected = True
        isWithinEpsilon epsilon actual expected = let diff = abs (actual - expected)
                                                      minNormal = fromIntegral (fst (floatRange actual))
                                                      maxValue = fromIntegral (snd (floatRange actual)) in
                                                      if actual == 0 || expected == 0 || diff < minNormal then diff < epsilon * minNormal else diff / min (abs actual + abs expected) maxValue < epsilon
