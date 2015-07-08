{-|
Module      : Test.Problem.Instances
Description : Tests for the Instances of IsProblem
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports the tests for the Instances module and its sub-modules
-}

module Test.Problem.Instances (
  tests -- TestTree
  ) where

import qualified Test.Problem.Instances.CNF as CNF
import           TestUtils

name :: String
name = "Instances"

tests :: TestTree
tests =
  testGroup name [
    CNF.tests
    ]
