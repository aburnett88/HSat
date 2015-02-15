module TestUtils.Problem.BSP.CNF (
  mkArbCNF,
  mkSmallCNF
  ) where

import TestUtils.Problem.BSP.CNF.Builder
import TestUtils.Test
import HSat.Problem.BSP.CNF
import HSat.Problem.BSP.Common.Clauses
import TestUtils.Problem.BSP.Common.Clauses

instance Arbitrary CNF where
  arbitrary = mkArbCNF arbitrary
  shrink cnf =
    map mkCNFFromClauses $ shrink . getClauses $ cnf

mkArbCNF :: Gen Clauses -> Gen CNF
mkArbCNF = liftM mkCNFFromClauses


--Makes a CNF with no more than 100 variables overlal, shrunk to its smallest form
mkSmallCNF :: Gen CNF
mkSmallCNF = mkArbCNF mkSmallClauses
