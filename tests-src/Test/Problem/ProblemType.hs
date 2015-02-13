{-|
Module      : Test.Problem.ProblemType
Description : The ProblemType test node
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Provides the tests for the ProblemExpr type
-}


module Test.Problem.ProblemType (
  tests,
  printer
  ) where

import TestUtils
import HSat.Problem.ProblemType

name :: String
name = "ProblemType"

tests :: TestTree
tests = testGroup name []

printer :: TestTree
printer =
  testGroup name [
    printProblemTypeArbitrary
    ]

printProblemTypeArbitrary :: TestTree
printProblemTypeArbitrary =
  printTest "ProblemType" (
    (generate arbitrary) :: IO ProblemType)
