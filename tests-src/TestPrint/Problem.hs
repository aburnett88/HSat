{-|
Module      : TestPrinter.Problem
Description : The Printer tests for the Problem Test Node
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the Test Node for the Problem module Printer tests and its children
-}

module TestPrint.Problem (
  printer
  ) where

import TestUtils
import HSat.Problem
import qualified TestPrint.Problem.ProblemExpr as ProblemExpr
import qualified TestPrint.Problem.ProblemType as ProblemType
import qualified TestPrint.Problem.Source as Source

name :: String
name = "Problem"

printer :: TestTree
printer =
  testGroup name [
    printProblemArbitrary,
    ProblemExpr.printer,
    ProblemType.printer,
    Source.printer
    ]

printProblemArbitrary :: TestTree
printProblemArbitrary =
  printTest "Problem" (
    (generate arbitrary) :: IO Problem)
