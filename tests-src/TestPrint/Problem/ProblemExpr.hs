{-|
Module      : TestPrint.Problen.ProblemExpr
Description : The ProblemExpr type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the ProblemExpr type's Printer Tests
-}

module TestPrint.Problem.ProblemExpr (
  printer,     -- :: TestTree
  problemExprs -- :: [ProblemExpr]
  ) where

import           HSat.Problem.ProblemExpr
import           TestPrint
import qualified TestPrint.Problem.BSP.CNF as CNF
import qualified TestPrint.Problem.BSP.Common as Common


name :: String
name = "ProblemExpr"

printer :: TestTree
printer =
  testGroup name [
    printProblemExpr,
    Common.printer,
    CNF.printer
    ]

printProblemExpr :: TestTree
printProblemExpr =
  printList "ProblemExpr CNF" problemExprs

problemExprs :: [ProblemExpr]
problemExprs =
  map CNFExpr CNF.cnfList
