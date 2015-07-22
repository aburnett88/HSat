module Test.Make.Instances (
  tests
  ) where

import TestUtils
import qualified Test.Make.Instances.CNF as CNF

name :: String
name = "Instances"

tests :: TestTree
tests =
  testGroup name [
    CNF.tests
    ]
