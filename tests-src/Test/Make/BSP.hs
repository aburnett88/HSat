module Test.Make.BSP (
  tests
  ) where

import TestUtils

name :: String
name = "BSP"

tests :: TestTree
tests =
  testGroup name []
