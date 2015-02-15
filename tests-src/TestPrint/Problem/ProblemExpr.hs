{-|
Module      : TestPrint.Problem.ProblemExpr
Description : The ProblemExpr Printing test Node
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

The ProblemExpr test Printing node and associated Leaves
-}

module TestPrint.Problem.ProblemExpr (
  printer
  ) where

import HSat.Problem.ProblemExpr
import qualified TestPrint.Problem.BSP.CNF as CNF
import qualified TestPrint.Problem.BSP.Common as Common
import TestUtils


name :: String
name = "ProblemExpr"

printer :: TestTree
printer =
  testGroup name [
    printProblemExprArbitrary,
    Common.printer,
    CNF.printer
    ]

printProblemExprArbitrary :: TestTree
printProblemExprArbitrary =
  printTest "ProblemExpr" (
    (generate arbitrary) :: IO ProblemExpr)
