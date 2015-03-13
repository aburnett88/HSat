{-|
Module      : Test.Problen.BSP.CNF
Description : The CNF TestTree Node
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Node for the CNF module
-}

module Test.Problem.BSP.CNF (
  tests
  ) where

import           HSat.Problem.BSP.CNF
import qualified Test.Problem.BSP.CNF.Builder as CNFBuilder
import qualified Test.Problem.BSP.CNF.Internal as Internal
import           TestUtils
import TestUtils.Validate

name :: String
name = "CNF"

tests :: TestTree
tests = testGroup name [
  Internal.tests,
  CNFBuilder.tests,
  testCNF,
  testGroup "mkCNFFromClauses" [
     mkCNFFromClausesTest1
     ],
  testGroup "cnfToIntegers" [
     cnfToIntegersTest1
     ],
  testGroup "mkCNFFromIntegers" [
    mkCNFFromIntegersTest1
    ]
  ]

testCNF :: TestTree
testCNF =
  testProperty "Arbitrary CNF are valid" $ property testCNF'
  where
    testCNF' :: CNF -> Bool
    testCNF' = validate 

mkCNFFromClausesTest1 :: TestTree
mkCNFFromClausesTest1 =
  testProperty "mkCNFFromClauses has correct values" $ property
  (\clauses ->
    let gottenClauses = getClauses $ mkCNFFromClauses clauses
    in gottenClauses  === clauses
  )

cnfToIntegersTest1 :: TestTree
cnfToIntegersTest1 =
  testProperty "mkCNFFromInegers . cnfToIntegers cnf == cnf" $ property
  (\cnf ->
    let cnf'               = mkCNFFromIntegers $ cnfToIntegers cnf
        expectedClauseNumb = getClauseNumb cnf
        gottenClauseNumb   = getClauseNumb cnf'
        expectedClauses    = getClauses cnf
        gottenClauses      = getClauses cnf'
        expectedVarNumb    = getMaxVar cnf
        gottenVarNumb      = getMaxVar cnf'
    in (expectedClauseNumb === gottenClauseNumb) .&&.
       (expectedClauses    === gottenClauses   ) .&&.
       (expectedVarNumb    >=  gottenVarNumb   )
  )

mkCNFFromIntegersTest1 :: TestTree
mkCNFFromIntegersTest1 =
  testProperty "cnfToIntegers . mkCNFFromIntegers $ ints == ints" $
  forAll
  (listOf $ listOf mkIntegerNonZero)
  (\ints ->
    ints === cnfToIntegers (mkCNFFromIntegers ints)
    )

