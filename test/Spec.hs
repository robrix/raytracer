import Test.Hspec
import Geometry.Ray.Spec
import Geometry.Vector.Spec

main :: IO ()
main = hspec $ do
  describe "Geometry.Ray" Geometry.Ray.Spec.spec
  describe "Geometry.Vector" Geometry.Vector.Spec.spec
