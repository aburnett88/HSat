{-|
Module      : TestPrint.Problen.Source
Description : The Source PrintTest Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for Source Test Printing
-}

module TestPrint.Problem.Source (
  printer
  ) where

import HSat.Problem.Source
import TestUtils

name :: String
name = "Source"

printer :: TestTree
printer =
  testGroup name [
    printSourceArbitrary
    ]

printSourceArbitrary :: TestTree
printSourceArbitrary =
  printTest "Source" (
    (generate arbitrary) :: IO Source)
