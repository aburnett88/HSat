module Test.Make.Config (
  tests
  ) where

import TestUtils
import qualified Test.Make.Config.Class as Class

name :: String
name = "Config"

tests :: TestTree
tests =
  testGroup name [
    Class.tests
    ]
