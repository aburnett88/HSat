{-|
Module      : TestPrint.Problen.BSP.Common.Literal
Description : The Literal PrintTest Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for Literal Test Printing
-}

module TestPrint.Problem.BSP.Common.Literal (
  printer
  ) where

import HSat.Problem.BSP.Common.Literal
import TestUtils

name :: String
name = "Literal"

printer :: TestTree
printer =
  testGroup name [
    printLiteralArbitrary
    ]

printLiteralArbitrary :: TestTree
printLiteralArbitrary =
  printTest "Literal" (
    (generate arbitrary) :: IO Literal)
