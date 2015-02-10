module Test.Make.Config (
  tests
  ) where

import TestUtils

name :: String
name = "Config"

tests :: TestTree
tests =
  testGroup name []
