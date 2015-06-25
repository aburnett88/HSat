{-|
Module      : TestPrint.Problen
Description : The Problem type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Problem type's Printer Tests
-}

module TestPrint.Problem (
  printer -- :: TestTree
  ) where

import           HSat.Problem
import           TestPrint
import qualified TestPrint.Problem.ProblemExpr as ProblemExpr
import qualified TestPrint.Problem.Source as Source
import qualified TestPrint.Problem.Instances as Instances

name :: String
name = "Problem"

printer :: TestTree
printer =
  testGroup name [
    printProblems,
    ProblemExpr.printer,
    Instances.printer,
    Source.printer
    ]

printProblems :: TestTree
printProblems =
  printList "Problem" $ map (\(p,s) -> MkProblem s p) $
  zip ProblemExpr.problemExprs Source.sources
