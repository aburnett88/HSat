module Test.Make.Common (
  tests
  ) where

import TestUtils

name :: String
name = "Common"

tests :: TestTree
tests =
  testGroup name []
