{-|
Module      : TestPrint.Problen.BSP.Common.Clause
Description : The Clause type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Clause type's Printer Tests
-}

module TestPrint.Problem.BSP.Common.Clause (
  printer -- :: TestTree
  ) where

import HSat.Problem.BSP.Common
import TestPrint

name :: String
name = "Clause"

printer :: TestTree
printer =
  testGroup name [
    testPrintEmpty,
    testPrintLongVars,
    testPrintLong,
    testPrintNormal,
    testPrintAllLong
    ]

testPrintEmpty :: TestTree
testPrintEmpty =
  printTest "Empty Clause" emptyClause

testPrintLongVars :: TestTree
testPrintLongVars =
  printTest "Long variables in Clause" $
  mkClauseFromIntegers [
    10000000000000000000000000000000000000000000000000000,
    -22222222222222222222222222222222222222222222222222222222222222,
    44444444444444444444444444444444444444444444444444444444444444444,
    -888888888888888888888888888888888888888888888888888888888,
    1010101010101010101010111111111111111111111111111111111111,
    -3456425456456456456456456456456456456456456456456456456456
    ]

testPrintLong :: TestTree
testPrintLong =
  printTest "Long Clause" $
  mkClauseFromIntegers $ replicate 100 l ++ replicate 100 (negate l)
  where
    l = 1345

testPrintNormal :: TestTree
testPrintNormal =
  printTest "Normal Clause" $
  mkClauseFromIntegers [
    100,785,452,52,-358,-247,-572,-754
                                  ]

testPrintAllLong :: TestTree
testPrintAllLong =
  printTest "Long Clauses and Variables" $
  mkClauseFromIntegers $ replicate 100 l ++ replicate 100 (negate l)
  where
    l = 1345897234895672345897234895723489578923572435234567
