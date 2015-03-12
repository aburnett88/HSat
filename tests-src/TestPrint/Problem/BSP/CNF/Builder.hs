{-|
Module      : TestPrint.Problen.BSP.CNF.Builder
Description : The CNFBuilder module Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the CNFBuilder module Printer Tests
-}

module TestPrint.Problem.BSP.CNF.Builder (
  printer -- :: TestTree
  ) where

import HSat.Problem.BSP.CNF.Builder.Internal
import HSat.Problem.BSP.Common
import TestPrint

name :: String
name = "Builder"

printer :: TestTree
printer =
  testGroup name [
    printBuilders,
    printErrors1,
    printErrors2,
    printErrors3
    ]

printBuilders :: TestTree
printBuilders =
  printList "CNFBuilder" [
    CNFBuilder 234 200 6 clauses clause,
    CNFBuilder 234 268 5 clauses emptyClause,
    CNFBuilder 10 30 0 emptyClauses emptyClause
    ]

clauses :: Clauses
clauses = mkClausesFromIntegers $ [
  [-24,-185,63,56,46,32,4,-47,-5,-1],
  [134,45,63,67,88,-91,100],
  [1,1,1,1,1],
  [2,2,2,2,-2],
  [44,-44,33]
  ]

clause :: Clause
clause = mkClauseFromIntegers $ [
  23,-65,11,199,-198]

printErrors1 :: TestTree
printErrors1 =
  printTest "Incorrect Clause Number" (
    IncorrectClauseNumber 10 8)

printErrors2 :: TestTree
printErrors2 =
  printTest "Incorrect Var Range" (
    VarOutsideRange 100000 1000023434)

printErrors3 :: TestTree
printErrors3 =
  printTest "Initialisation Error" (
    Initialisation (-1) (-1))
