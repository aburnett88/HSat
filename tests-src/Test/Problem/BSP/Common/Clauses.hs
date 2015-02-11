{-|
Module      : Test.Problen.BSP.Common.Clauses
Description : The 'Clause' test leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The tests for the 'Clauses' data type
-}

module Test.Problem.BSP.Common.Clauses (
  tests
  ) where

import TestUtils
import HSat.Problem.BSP.Common.Clauses
import HSat.Problem.BSP.Common.Clause (emptyClause)
import Control.Monad (replicateM)
import qualified Data.Vector as V

name :: String
name = "Clauses"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkClauses" [
       mkClausesTest1,
       mkClausesTest2
       ],
    testGroup "mkClausesFromClause" [
       mkClausesFromClauseTest1
       ],
    testGroup "clausesAddClause" [
       clausesAddClauseTest1
       ],
    testGroup "mkClausesFromIntegers" [
       mkClausesFromIntegersTest1
       ],
    testGroup "clausesToIntegers" [
       clausesToIntegersTest1
       ],
    testGroup "clausesIsEmpty" [
       clausesIsEmptyTest1,
       clausesIsEmptyTest2
       ]
    ]

mkClausesTest1 :: TestTree
mkClausesTest1 =
  testCase "0 == numberOfClauses mkClause" $ assert (
    0 == getSizeClauses mkClauses
    )

mkClausesTest2 :: TestTree
mkClausesTest2 =
  testCase "[] == getVectOfClauses mkClauses" $ assert (
    V.empty == getVectClause mkClauses
    )

mkClausesFromClauseTest1 :: TestTree
mkClausesFromClauseTest1 =
  testProperty "toList . getvect . mkClausesFromClause c == c" $ property (
    \cl ->
    cl == (V.toList . getVectClause . mkClausesFromClause $ cl)
    )

clausesAddClauseTest1 :: TestTree
clausesAddClauseTest1 =
  testProperty "clausesAddClause cl c == cl ++c" $ property (
    \(clauses,clause) ->
    clausesAddClause clauses clause ==
      mkClausesFromClause ((V.toList . getVectClause $ clauses) ++ [clause]
      )
    )

mkClausesFromIntegersTest1 :: TestTree
mkClausesFromIntegersTest1 =
  testProperty "clausesToIntegers . mkClausesFromIntegers == id" $
  forAll
  (choose (0,testMaxClausesSize) >>= flip replicateM (choose (0,testMaxClauseSize) >>=
                                      flip replicateM mkIntegerNonZero))
  (\ints ->
    ints == (clausesToIntegers . mkClausesFromIntegers $ ints)
    )

clausesToIntegersTest1 :: TestTree
clausesToIntegersTest1 =
  testProperty "mkClausesFromIntgers . clausesToIntegers == id" $ property (
    \clauses ->
    clauses == (mkClausesFromIntegers . clausesToIntegers $ clauses)
    )

clausesIsEmptyTest1 :: TestTree
clausesIsEmptyTest1 =
  testCase "clausesIsEmpty []" $ assert (
    clausesIsEmpty mkClauses
    )

clausesIsEmptyTest2 :: TestTree
clausesIsEmptyTest2 =
  testCase "clausesIsEmpty [..]" $ assert (
    not . clausesIsEmpty . mkClausesFromClause $ [emptyClause]
    )
