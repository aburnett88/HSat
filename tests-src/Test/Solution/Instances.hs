{-|
Module      : Test.Solution.Instances
Description : Tests the Solution Instances module and its sub-modules
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Solution Instances modules
-}

module Test.Solution.Instances (
  tests,-- :: TestTree
  ) where

import qualified Test.Solution.Instances.CNF as CNF
import           TestUtils

name :: String
name = "Instances"

tests :: TestTree
tests =
  testGroup name [
    CNF.tests
    ]
