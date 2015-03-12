{-|
Module      : TestUtils.Problem.BSP.CNF
Description : Generators for the CNF type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports Generator functions for the CNF type
-}

module TestUtils.Problem.BSP.CNF (
  genCNF -- :: Int -> Gen CNF
  ) where

import           TestUtils.Test
import           HSat.Problem.BSP.CNF
import           HSat.Problem.BSP.Common.Clauses
import           HSat.Problem.BSP.CNF.Internal
import           TestUtils.Problem.BSP.Common.Clauses
import           TestUtils.Validate

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
    let maxVar  = getMaxVar cnf
        clauses = getClauses cnf
        cnfFunc = \cl -> CNF maxVar (getSizeClauses cl) cl
    in map cnfFunc $ shrink clauses

instance Validate CNF where
  validate (CNF maxVar clauseNumb clauses) =
    let valClauseNumb = getSizeClauses clauses
        valMaxVar     = findMaxVar clauses
    in (valClauseNumb == clauseNumb) &&
       (valMaxVar == maxVar)
