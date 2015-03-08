{-|
Module      : Test.Problen.BSP.CNF.Internal
Description : The 'Internal' CNF Test Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Internal CNF module
-}

module Test.Problem.BSP.CNF.Internal (
  tests
  ) where

import TestUtils
import HSat.Problem.BSP.CNF.Internal
import HSat.Problem.BSP.Common
import TestUtils.Validate
import HSat.Problem.BSP.CNF

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    cnfTest1,
    cnfTest2
    ]

cnfTest1 :: TestTree
cnfTest1 =
  testProperty "Arbitrary created CNFs are valid" $ property cnfTest
  where
    cnfTest :: CNF -> Bool
    cnfTest = validate

cnfTest2 :: TestTree
cnfTest2 =
  testProperty "CNF Tests with variable outside range are not valid" $
  forAll
  (do
      let headLit    = mkLiteral pos (mkVariable 1)
          headClause = clauseAddLiteral emptyClause headLit
      clauses <- arbitrary
      let allClauses = clausesAddClause clauses headClause
          cnf        = mkCNFFromClauses allClauses
      return cnf
      )
  --Guaranteed to have atleast a lieral of 1 in it
  (\cnf ->
    let clauses    = getClauses cnf
        maxVar     = getMaxVar cnf
        clauseNumb = getClauseNumb cnf
        newMaxVar  = maxVar `div` 2
        newCNF     = CNF newMaxVar clauseNumb clauses
    in not $ validate newCNF
  )
