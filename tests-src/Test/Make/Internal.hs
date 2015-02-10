module Test.Make.Internal (
  tests
  ) where

import TestUtils

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name []
