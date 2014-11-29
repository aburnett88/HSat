import TestUtils
import qualified Test.Data as Data
import qualified Test.Utils as Utils

main :: IO ()
main = defaultMain tests

name :: String
name = "HSat Tests"

tests :: TestTree
tests =
  testGroup name [
    Data.tests,
    Utils.tests
    ]
