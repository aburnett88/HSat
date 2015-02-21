module Test.Make.BSP.Common.Clause (
  tests
  ) where

import TestUtils

name :: String
name = "Clause"

tests :: TestTree
tests =
  testGroup name []
