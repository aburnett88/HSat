module Test.Make.Config.Class (
  tests
  ) where

import TestUtils
import HSat.Make.Config.Class

name :: String
name = "Class"

tests :: TestTree
tests =
  testGroup name []

instance Arbitrary Config where
  arbitrary = undefined
  shrink _ = []
