import Test.Hspec
import Geometry.Vector.Spec

main :: IO ()
main = hspec $ do
  describe "Geometry.Vector" Geometry.Vector.Spec.spec
