{-|
Module      : Test.Problem.ProblemExpr
Description : The ProblemExpr test node and associated tests
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Provides the tests for the ProblemExpr type and its children types
-}

module Test.Problem.ProblemExpr (
  tests
  ) where

import           TestUtils
import qualified Test.Problem.ProblemExpr.Class as Class

name :: String
name = "ProblemExpr"

tests :: TestTree
tests =
  testGroup name [
    Class.tests
    ]
