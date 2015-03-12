{-|
Module      : TestPrint.Problen.ProblemType
Description : The ProblemType type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the ProblemType type's Printer Tests
-}

module TestPrint.Problem.ProblemType (
  printer -- :: TestTree
  ) where

import HSat.Problem.ProblemType
import TestPrint

name :: String
name = "ProblemType"

printer :: TestTree
printer =
  testGroup name [
    problemTypes
    ]

problemTypes :: TestTree
problemTypes =
  printList "Problem Type" [CNF]
