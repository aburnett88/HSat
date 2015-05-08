{-# LANGUAGE RecordWildCards #-}

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
    \(source',problem) ->
    source' === source (mkProblem source' problem)
    )

mkProblemTest2 :: TestTree
mkProblemTest2 =
  testProperty "getProblemExpr . mkProblem s p == p" $ property (
    \(source,problem) ->
    problem == problemExpr (mkProblem source problem)
    )

instance Arbitrary Problem where
  arbitrary = liftA2 mkProblem arbitrary arbitrary
  shrink Problem{..} =
    map (uncurry mkProblem) $ shrink (source,problemExpr)
