module Test.Make.Instances (
  tests
  ) where

import TestUtils

name :: String
name = "Instances"

tests :: TestTree
tests =
  testGroup name []
