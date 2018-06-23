import Test.Hspec
import Geometry.Ray.Spec

main :: IO ()
main = hspec $ do
  describe "Geometry.Ray" Geometry.Ray.Spec.spec
