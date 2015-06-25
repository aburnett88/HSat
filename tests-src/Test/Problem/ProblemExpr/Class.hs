module Test.Problem.ProblemExpr.Class (
  tests
  ) where

import TestUtils
import HSat.Problem.ProblemExpr.Class

name :: String
name = "Class"

tests :: TestTree
tests =
  testGroup name []

instance Arbitrary ProblemExpr where
  arbitrary = undefined
  shrink _ = []
