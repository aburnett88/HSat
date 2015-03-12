{-|
Module      : TestPrint.Problen.BSP.Common.Variable
Description : The Variable type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Variable type's Printer Tests
-}

module TestPrint.Problem.BSP.Common.Variable (
  printer -- :: TestTree
  ) where

import HSat.Problem.BSP.Common.Variable
import TestPrint

name :: String
name = "Variable"

printer :: TestTree
printer =
  testGroup name [
    printList "Variable" $ map mkVariable [
       1,10,100,1000,10000,100000,1000000000000,
       123452456,134753,23845653274556,85]
    ]
