{-|
Module      : TestPrint.Problen.ProblemType
Description : The ProblemType PrintTest Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for ProblemType Test Printing
-}

module TestPrint.Problem.ProblemType (
  printer
  ) where

import HSat.Problem.ProblemType
import TestUtils

name :: String
name = "ProblemType"

printer :: TestTree
printer =
  testGroup name [
    printProblemTypeArbitrary
    ]

printProblemTypeArbitrary :: TestTree
printProblemTypeArbitrary =
  printTest "ProblemType" (
    (generate arbitrary) :: IO ProblemType)
