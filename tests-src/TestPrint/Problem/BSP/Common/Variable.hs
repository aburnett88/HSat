{-|
Module      : TestPrint.Problen.BSP.Common.Variable
Description : The Variable PrintTest Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for Variable Test Printing
-}

module TestPrint.Problem.BSP.Common.Variable (
  printer
  ) where

import HSat.Problem.BSP.Common.Variable
import TestUtils

name :: String
name = "Variable"

printer :: TestTree
printer =
  testGroup name [
    printVariableArbitrary
    ]

printVariableArbitrary :: TestTree
printVariableArbitrary =
  printTest "Variable" (
    (generate arbitrary) :: IO Variable)
