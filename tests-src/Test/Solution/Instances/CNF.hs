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
import HSat.Problem.Instances.CNF
import Control.Monad (replicateM)
import qualified Data.Map as M (toList)
import Data.Maybe (isJust, isNothing)
import Prelude hiding (lookup)
import HSat.Problem.Instances.Common

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    testGroup "checkCNFSolution" [
       checkCNFSolutionTest1,
       checkCNFSolutionTest2,
       checkCNFSolutionTest3,
       checkCNFSolutionTest4
       ],
    testGroup "emptySolution" [
       emptySolutionTest1
       ],
    testGroup "mkTrueSet" [
      mkTrueSetTest1
      ],
    testGroup "lookup" [
      lookupTest1,
      lookupTest2
      ]
    ]

genBoolSolution :: Word -> Int -> Gen BoolSolution
genBoolSolution w _ =
  solutionFromList <$> replicateM (fromEnum w + 1) arbitrary

checkCNFSolutionTest1 :: TestTree
checkCNFSolutionTest1 =
  testCase "checkCNFSolution on known solution is True" $ do
    let cnf = mkCNFFromIntegers [[1,2,3],[-1,-2,3],[4,5,-1],[2,3],[1]]
        sol = solutionFromList [True,True,True,True,True]
    checkCNFSolution cnf sol @=? True

checkCNFSolutionTest2 :: TestTree
checkCNFSolutionTest2 =
  testCase "checkCNFSolution on empty CNF" $ do
    let cnf = mkCNFFromIntegers []
        sol = solutionFromList []
    checkCNFSolution cnf sol @=? True

checkCNFSolutionTest3 :: TestTree
checkCNFSolutionTest3 =
  testCase "checkCNFSolution on empty clause" $ do
    let cnf = mkCNFFromIntegers [[]]
        sol = solutionFromList []
    checkCNFSolution cnf sol @=? False

checkCNFSolutionTest4 :: TestTree
checkCNFSolutionTest4 =
  testCase "checkCNFSolution known invalid solution" $ do
    let cnf = mkCNFFromIntegers [[1,2,3],[-1,2,-3]]
        sol = solutionFromList [True,False,True]
    checkCNFSolution cnf sol @=? False

emptySolutionTest1 :: TestTree
emptySolutionTest1 =
  testCase "emptySolution has no elements" $ do
    let sol = M.toList . solution $ emptySolution
    null sol @=? True

mkTrueSetTest1 :: TestTree
mkTrueSetTest1 =
  testProperty "mkTrueSet n creates a solution of n elements" $ monadicIO $ do
    n   <- pick $ sized (\i -> toEnum <$> choose (0,i))
    sol <- run $ mkTrueSet n
    stop (
      (length . M.toList . solution $ sol) === (fromEnum n)
      )

lookupTest1 :: TestTree
lookupTest1 =
  testProperty "lookup vars in range returns correct value" $
  forAll
  (
    do
      size  <- arbitrary
      sol   <- sized $ genBoolSolution size
      index <- mkVariable <$> choose (1,size)
      return (sol,index)
  )
  (\(sol,index) ->
    property $ isJust $ lookup index sol
  )

lookupTest2 :: TestTree
lookupTest2 =
  testProperty "lookup vars outisde of range returns Nothing" $
  forAll
  (
    do
      size  <- arbitrary
      sol   <- sized $ genBoolSolution size
      index <- mkVariable <$> choose (1,size)
      return (sol,index)
  )
  (\(sol,index) ->
    property $ isNothing $ lookup index sol
  )
