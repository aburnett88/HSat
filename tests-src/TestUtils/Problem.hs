module TestUtils.Problem (

  ) where

import TestUtils.Test
import HSat.Problem
import HSat.Problem.Source
import HSat.Problem.ProblemExpr
import TestUtils.Problem.ProblemExpr ()
import TestUtils.Problem.Source ()
import TestUtils.Problem.ProblemType ()

instance Arbitrary Problem where
  arbitrary = mkArbProblem arbitrary arbitrary
  shrink problem =
    let source      = getSource problem
        problemExpr = getProblemExpr problem
    in map (uncurry mkProblem) $ shrink (source,problemExpr)

mkArbProblem :: Gen Source -> Gen ProblemExpr -> Gen Problem
mkArbProblem = liftM2 mkProblem
