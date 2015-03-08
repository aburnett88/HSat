{-|
Module      : TestPrint.Problen.BSP.Common.Sign
Description : The Sign Printer Tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the printing tests for Sign
-}

module TestPrint.Problem.BSP.Common.Sign (
  printer
  ) where

import HSat.Problem.BSP.Common.Sign
import TestUtils

name :: String
name = "Sign"

printer :: TestTree
printer =
  testGroup name [
    printSignPos,
    printSignNeg
    ]

printSignPos :: TestTree
printSignPos =
  printTest "Positive Sign"
  (return pos)

printSignNeg :: TestTree
printSignNeg =
  printTest "Negative Sign"
  (return neg)
