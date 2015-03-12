module TestUtils.Problem.ProblemExpr (
  mkArbCNFProblem
  ) where

import HSat.Problem.ProblemExpr
import HSat.Problem.BSP.CNF
import TestUtils.Test
import TestUtils.Problem.BSP.CNF ()
import TestUtils.Validate

instance Validate ProblemExpr where
  validate (CNFExpr cnf) = validate cnf

instance Arbitrary ProblemExpr where
  arbitrary = oneof [
    mkArbCNFProblem arbitrary
    ]
  shrink (CNFExpr cnf) =
    map mkCNFProblem $ shrink cnf

mkArbCNFProblem :: Gen CNF -> Gen ProblemExpr
mkArbCNFProblem = liftM mkCNFProblem
