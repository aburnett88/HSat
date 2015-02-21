module Test.Make.BSP.Common.Clauses (
  tests
  ) where

import TestUtils

name :: String
name = "Clauses"

tests :: TestTree
tests =
  testGroup name []
