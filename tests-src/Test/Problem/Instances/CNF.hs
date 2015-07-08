{-|
Module      : Test.Problem.Instances.CNF
Description : Tests for the CNF data-type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports the tests for the CNF module and its sub-modules
-}

module Test.Problem.Instances.CNF (
  tests -- TestTree
  ) where

import           HSat.Problem.Instances.CNF
import qualified Test.Problem.Instances.CNF.Builder  as Builder
import qualified Test.Problem.Instances.CNF.Internal as Internal
import qualified Test.Problem.Instances.CNF.Parser   as Parser
import qualified Test.Problem.Instances.CNF.Writer   as Writer
import qualified Test.Problem.Instances.Common       as Common  ()
import           TestUtils

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    Builder.tests ,
    Internal.tests,
    Parser.tests  ,
    Writer.tests  ,
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


mkCNFFromClausesTest1 :: TestTree
mkCNFFromClausesTest1 =
  testProperty ("getClauses . mkCNFFromClauses " `equiv` " id") $ property
  (\clauses ->
    let gottenClauses = getClauses $ mkCNFFromClauses clauses
    in gottenClauses  === clauses
  )

cnfToIntegersTest1 :: TestTree
cnfToIntegersTest1 =
  testProperty ("mkCNFFromInegers . cnfToIntegers cnf  " `equiv` " cnf") $ property
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
       counterexample "expectedVarNumb less than gottenVarNumb" (
         expectedVarNumb    >=  gottenVarNumb
         )
  )

mkCNFFromIntegersTest1 :: TestTree
mkCNFFromIntegersTest1 =
  testProperty ("cnfToIntegers . mkCNFFromIntegers $ ints " `equiv` " ints") $
  forAll
  (listOf $ listOf mkIntegerNonZero)
  (\ints ->
    ints === cnfToIntegers (mkCNFFromIntegers ints)
  )
