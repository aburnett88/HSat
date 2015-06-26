{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : Test.Problem.Internal
Description : The Test node for the Internal Problem module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the node for the tests of the Internal Problem module and its children
-}

module Test.Problem.Internal (
  tests
  ) where

import Control.Applicative
import HSat.Problem.Internal
import Test.Problem.ProblemExpr.Class ()
import Test.Problem.Source            ()
import TestUtils

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkProblem" [
       mkProblemTest1,
       mkProblemTest2
       ]
    ]

mkProblemTest1 :: TestTree
mkProblemTest1 =
  testProperty ("source . MkProblem s p " `equiv` " s") $ property (
    \(source',problem) ->
    source' === source (MkProblem source' problem)
    )

mkProblemTest2 :: TestTree
mkProblemTest2 =
  testProperty ("problemExpr . MkProblem s p " `equiv` " p") $ property (
    \(source',problem) ->
    problem === problemExpr (MkProblem source' problem)
    )

instance Arbitrary Problem where
  arbitrary            = liftA2 MkProblem arbitrary arbitrary
  shrink MkProblem{..} =
    map (uncurry MkProblem) $ shrink (source,problemExpr)
