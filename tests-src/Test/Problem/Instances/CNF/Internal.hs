{-# LANGUAGE
    RecordWildCards
    #-}

{-|
Module      : Test.Problen.Instances.CNF.Internal
Description : The tests for the Internal CNF module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Node for the Internal CNF module
-}

module Test.Problem.Instances.CNF.Internal (
  tests , -- TestTree
  genCNF, -- Int -> Gen CNF
  ) where

import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.CNF.Internal
import HSat.Problem.Instances.Common
import Test.Problem.Instances.Common.Clauses hiding (tests)
import TestUtils
import TestUtils.Validate


name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    testCNF
    ]

testCNF :: TestTree
testCNF =
  testProperty "Arbitrary CNF are valid" $ property testCNF'
  where
    testCNF' :: CNF -> Bool
    testCNF' = validate 

{-
The process to generate a valid CNF is rather complicated, and has been
annotated to ensure that no errors are made
-}
genCNF :: Int -> Gen CNF
genCNF sizeVal = do
  --Choose the set maximum variable
  setMaxVar <- choose (1,sizeVal)
  --Generate the clauses
  clauses   <- genClauses (toEnum setMaxVar) sizeVal
  --find the maximum variable in the clauses
  let mVar  = findMaxVar clauses
  --choose an offset for the set maxVar in the CNF
  offSet    <- choose (0,toEnum sizeVal)
  --Create the CNF
  let cnf   = CNF (mVar + offSet) (getSizeClauses clauses) clauses
  return cnf

instance Arbitrary CNF where
  arbitrary = sized genCNF
  shrink cnf =
    let maxVar     = getMaxVar cnf
        clauses    = getClauses cnf
        cnfFunc cl = CNF maxVar (getSizeClauses cl) cl
    in map cnfFunc $ shrink clauses

instance Validate CNF where
  validate CNF{..} =
    let valClauseNumb = getSizeClauses getClauses
        valMaxVar     = findMaxVar getClauses
    in (valClauseNumb == getClauseNumb) &&
       (valMaxVar <= getMaxVar) &&
       validate getClauses
