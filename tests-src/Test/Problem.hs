

{-|
Module      : Test.Problem
Description : The Test node for the Problem module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the node for the tests of the Problem type and its children
-}

module Test.Problem (
  tests
  ) where

import           HSat.Problem
import qualified Test.Problem.ProblemExpr as ProblemExpr
import qualified Test.Problem.Instances as Instances
import qualified Test.Problem.Source as Source
import           TestUtils
import HSat.Problem.Internal
import qualified Test.Problem.Internal as Internal

name :: String
name = "Problem"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkProblem" [
       mkProblemTest1,
       mkProblemTest2
       ],
    Internal.tests,
    ProblemExpr.tests,
    Source.tests,
    Instances.tests
    ]

mkProblemTest1 :: TestTree
mkProblemTest1 =
  testProperty "getSource . mkProblem s p == s" $ property (
    \(source',problem) ->
    source' === source (MkProblem source' problem)
    )

mkProblemTest2 :: TestTree
mkProblemTest2 =
  testProperty "getProblemExpr . mkProblem s p == p" $ property (
    \(source',problem) ->
    problem === problemExpr (MkProblem source' problem)
    )

