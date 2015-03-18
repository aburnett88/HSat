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
import qualified Test.Problem.ProblemType as ProblemType
import qualified Test.Problem.Source as Source
import           TestUtils
import HSat.Problem.Source
import HSat.Problem.ProblemExpr
import Control.Applicative

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

mkProblemTest1 :: TestTree
mkProblemTest1 =
  testProperty "getSource . mkProblem s p == s" $ property (
    \(source,problem) ->
    source === getSource (mkProblem source problem)
    )

mkProblemTest2 :: TestTree
mkProblemTest2 =
  testProperty "getProblemExpr . mkProblem s p == p" $ property (
    \(source,problem) ->
    problem == getProblemExpr (mkProblem source problem)
    )

instance Arbitrary Problem where
  arbitrary = mkArbProblem arbitrary arbitrary
  shrink problem =
    let source      = getSource problem
        problemExpr = getProblemExpr problem
    in map (uncurry mkProblem) $ shrink (source,problemExpr)

mkArbProblem :: Gen Source -> Gen ProblemExpr -> Gen Problem
mkArbProblem = liftA2 mkProblem
