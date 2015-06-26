{-|
Module      : Test.Problem
Description : The Test node for the Problem module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the node for the tests of the Problem module and its children
-}

module Test.Problem (
  tests
  ) where

import qualified Test.Problem.Instances   as Instances
import qualified Test.Problem.Internal    as Internal
import qualified Test.Problem.ProblemExpr as ProblemExpr
import qualified Test.Problem.Source      as Source
import           TestUtils

name :: String
name = "Problem"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests   ,
    ProblemExpr.tests,
    Source.tests     ,
    Instances.tests
    ]
