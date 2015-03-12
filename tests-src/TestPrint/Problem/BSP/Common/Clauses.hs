{-|
Module      : TestPrint.Problen.BSP.Common.Clauses
Description : The Clauses type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Clauses type's Printer Tests
-}

module TestPrint.Problem.BSP.Common.Clauses (
  printer -- :: TestTree
  ) where

import HSat.Problem.BSP.Common.Clauses
import TestPrint

name :: String
name = "Clauses"

printer :: TestTree
printer =
  testGroup name [
    printEmptyClauses,
    printManyClausesAllShort,
    printManyClausesAllLong,
    printManyClausesLongAndVarsLong,
    printManyClausesVarsLong,
    printNormalClauses
    ]

splitter :: Int -> [Integer] -> [[Integer]]
splitter n xs = if length xs < n then
                  [xs] else
                  (take n xs) : splitter n (drop n xs)

printEmptyClauses :: TestTree
printEmptyClauses =
  printTest "Empty Clauses" emptyClauses

printManyClausesAllShort :: TestTree
printManyClausesAllShort =
  printTest "Short Clauses" $
  mkClausesFromIntegers . (splitter 3) . filter (/=0) $ [-100..100]

printManyClausesAllLong :: TestTree
printManyClausesAllLong =
  printTest "Long Clauses" $
  mkClausesFromIntegers . (splitter 40) . filter (/=0) $ [-100..100]

printManyClausesLongAndVarsLong :: TestTree
printManyClausesLongAndVarsLong =
  printTest "Long clauses, vars long" $
  mkClausesFromIntegers . (splitter 40) $ [n .. (n + 500)] ++ [
    (negate n) .. (negate (n + 500))]
  where
    n = 134589734958734859

printManyClausesVarsLong :: TestTree
printManyClausesVarsLong =
  printTest "Many clauses vars long" $
  mkClausesFromIntegers . (splitter 3) $ [n .. (n + 50)] ++ [
    (negate n) .. (negate (n + 500))]
  where
    n = 345783495734895

printNormalClauses :: TestTree
printNormalClauses =
  printTest "Normal Clauses" $
  mkClausesFromIntegers [
    [134,656,-245,-345],
    [],
    [234,-3454,-3,-234,67,-67]
    ]
