{-|
Module      : TestPrint.Problen.BSP.Common.Sign
Description : The Sign type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Sign type's Printer Tests
-}

module TestPrint.Problem.BSP.Common.Sign (
  printer -- :: TestTree
  ) where

import HSat.Problem.BSP.Common.Sign
import TestPrint

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
  printTest "Positive Sign" pos

printSignNeg :: TestTree
printSignNeg =
  printTest "Negative Sign" neg
