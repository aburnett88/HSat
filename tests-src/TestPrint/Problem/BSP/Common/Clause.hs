{-|
Module      : TestPrint.Problen.BSP.Common.Clause
Description : The Clause PrintTest Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for Clause Test Printing
-}

module TestPrint.Problem.BSP.Common.Clause (
  printer
  ) where

import HSat.Problem.BSP.Common.Clause
import TestUtils

name :: String
name = "Clause"

printer :: TestTree
printer =
  testGroup name [
    printClauseArbitrary
    ]

printClauseArbitrary :: TestTree
printClauseArbitrary =
  printTest "Clause" (
    generate arbitrary :: IO Clause)
