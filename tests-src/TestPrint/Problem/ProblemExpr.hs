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

import           HSat.Problem.ProblemExpr.Class
import           TestPrint


name :: String
name = "ProblemExpr"

printer :: TestTree
printer =
  testGroup name [
    printProblemExpr
    ]

printProblemExpr :: TestTree
printProblemExpr =
  printList "ProblemExpr CNF" problemExprs

problemExprs :: [ProblemExpr]
problemExprs = []
