{-|
Module      : TestPrint.Problen.BSP.CNF
Description : The CNF PrintTest Node
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Node and associated Leaves for Printing Tests
-}

module TestPrint.Problem.BSP.CNF (
  printer
  ) where

import HSat.Problem.BSP.CNF
import qualified TestPrint.Problem.BSP.CNF.Builder as CNFBuilder
import TestUtils

name :: String
name = "CNF"

printer :: TestTree
printer =
  testGroup name [
    printCNFArbitrary,
    CNFBuilder.printer
    ]

printCNFArbitrary :: TestTree
printCNFArbitrary =
  printTest "CNF" (
    (generate arbitrary) :: IO CNF)
