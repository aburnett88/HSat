{-|
Module      : Test.Solution.Instances.CNF
Description : Tests the Solution Cnf module and its sub-modules
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Solution CNF modules
-}

module Test.Solution.Instances.CNF (
  tests          ,-- :: TestTree
  genBoolSolution,-- :: Word -> Int -> Gen BoolSolution
  ) where

import HSat.Solution.Instances.CNF
import TestUtils

tests :: TestTree
tests =
  testGroup name [
    testGroup "checkCNFSolution" [
       checkCNFSolutionTest1
       ]
    ]

genBoolSolution :: Word -> Int -> Gen BoolSolution
genBoolSolution = error "genBoolSolution not written"

checkCNFSolutionTest1 :: TestTree
checkCNFSolutionTest1 =
  testCase "checkCNFSolution on known solution is True" $ do
    let cnf = mkCNFFromIntegers [[1,2,3],[-1,-2,3],[4,5,-1],[2,3],[1]]
        sol = solutionFromList [True,True,True,True,True]
    genBoolSolution cnf sol @=? True
