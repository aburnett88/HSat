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
  tests,
  printer
  ) where

import           HSat.Problem
import qualified Test.Problem.ProblemExpr as ProblemExpr
import qualified Test.Problem.ProblemType as ProblemType
import qualified Test.Problem.Source as Source
import           TestUtils

name :: String
name = "Problem"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkProblem" [
       mkProblemTest1,
       mkProblemTest2
       ],
    ProblemExpr.tests,
    ProblemType.tests,
    Source.tests
    ]

printer :: TestTree
printer =
  testGroup name [
    testGroup "mkProblem" [
       printProblemArbitrary
       ]
    ]

printProblemArbitrary :: TestTree
printProblemArbitrary =
  printTest "Problem" (
    (generate arbitrary) :: IO Problem)

mkProblemTest1 :: TestTree
mkProblemTest1 =
  testProperty "getSource . mkProblem s p == s" $ property (
    \(source,problem) ->
    source == getSource (mkProblem source problem)
    )

mkProblemTest2 :: TestTree
mkProblemTest2 =
  testProperty "getProblemExpr . mkProblem s p == p" $ property (
    \(source,problem) ->
    problem == getProblemExpr (mkProblem source problem)
    )
