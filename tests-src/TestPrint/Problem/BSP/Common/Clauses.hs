{-|
Module      : TestPrint.Problen.BSP.Common.Clauses
Description : The Clauses PrintTest Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for Clauses Test Printing
-}

module TestPrint.Problem.BSP.Common.Clauses (
  printer
  ) where

import HSat.Problem.BSP.Common.Clauses
import TestUtils

name :: String
name = "Clauses"

printer :: TestTree
printer =
  testGroup name [
    printClausesArbitrary
    ]

printClausesArbitrary :: TestTree
printClausesArbitrary =
  printTest "Clauses" (
    (generate arbitrary) :: IO Clauses)
