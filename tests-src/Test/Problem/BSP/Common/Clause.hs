{-|
Module      : Test.Problem.BSP.Common.Clause
Description : The test leaf for the Clause type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the tests for the Clause type
-}

module Test.Problem.BSP.Common.Clause (
  tests
  ) where

import           Control.Monad (replicateM)
import qualified Data.Vector as V
import           HSat.Problem.BSP.Common.Clause
import           TestUtils

name :: String
name = "Clause"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkClause" [
       mkClauseTest1,
       mkClauseTest2
       ],
    testGroup "mkClauseFromIntegers" [
      mkClauseFromIntegersTest1
      ],
    testGroup "clauseAddLit" [
      clauseAddLitTest1
      ],
    testGroup "clauseToIntegers" [
      clauseToIntegersTest1
      ],
    testGroup "clauseIsEmpty" [
      clauseIsEmptyTest1,
      clauseIsEmptyTest2
      ]
    ]

mkClauseTest1 :: TestTree
mkClauseTest1 =
  testCase "clauseLength . mkClause == 0" $ assert (
    getSizeClause mkClause == 0
    )

mkClauseTest2 :: TestTree
mkClauseTest2 =
  testCase "getLiterals . mkClause == V.empty" $ assert (
    getLiterals mkClause == V.empty
    )

clauseToIntegersTest1 :: TestTree
clauseToIntegersTest1 =
  testProperty "mkClauseFromIntegers . clauseToIntegers == id" $ property (
    \clause ->
      clause == (mkClauseFromIntegers . clauseToIntegers $ clause)
    )

clauseAddLitTest1 :: TestTree
clauseAddLitTest1 =
  testProperty "addLit c l == mkClause c ++ [l]" $ property (
    \(clause,lit) ->
      mkClauseFromLits ((V.toList . getLiterals $ clause)++[lit]) ==
      clauseAddLiteral clause lit
      )

mkClauseFromIntegersTest1 :: TestTree
mkClauseFromIntegersTest1 =
  testProperty "clauseToIntegers . mkClauseFromIntegers == id" $
  forAll
  (choose (0,testMaxClauseSize) >>= flip replicateM mkIntegerNonZero)
  (\ints ->
    ints == (clauseToIntegers . mkClauseFromIntegers $ ints)
    )

clauseIsEmptyTest1 :: TestTree
clauseIsEmptyTest1 =
  testCase "clauseIsEmpty [] == True" $ assert (
    clauseIsEmpty mkClause
    )

clauseIsEmptyTest2 :: TestTree
clauseIsEmptyTest2 =
  testCase "clauseIsEmpty [1] == False" $ assert (
    not . clauseIsEmpty . mkClauseFromIntegers $ [-1,1]
    )
