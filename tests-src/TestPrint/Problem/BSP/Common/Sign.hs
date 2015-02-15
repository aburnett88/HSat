{-|
Module      : TestPrint.Problen.BSP.Common.Sign
Description : The Sign PrintTest Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for Sign Test Printing
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
    printSignArbitrary
    ]

printSignArbitrary :: TestTree
printSignArbitrary =
  printTest "Sign" (
    (generate arbitrary) :: IO Sign)
