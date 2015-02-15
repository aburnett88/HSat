module TestUtils.Problem.BSP.Common.Clauses (
  mkArbClauses,
  mkSmallClauses
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Clauses
import qualified Data.Vector as V
import TestUtils.Problem.BSP.Common.Clause
import TestUtils.Limits
import HSat.Problem.BSP.Common.Clause

instance Arbitrary Clauses where
  arbitrary = mkArbClauses arbitrary maxClauses
  shrink clauses =
    map mkClauses $ shrink . getVectClause $ clauses

mkArbClauses :: Gen Clause -> Int -> Gen Clauses
mkArbClauses genClause sizeBound = do
  size <- choose (0,sizeBound)
  liftM mkClauses (V.replicateM size genClause)

mkSmallClauses :: Gen Clauses
mkSmallClauses = mkArbClauses mkSmallClause 10
