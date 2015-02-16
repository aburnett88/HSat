module TestUtils.Problem.BSP.Common.Clauses (
  genClauses,
  genClausesValid,
  genClausesInvalid
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Clauses.Internal
import HSat.Problem.BSP.Common.Clauses
import qualified Data.Vector as V
import TestUtils.Problem.BSP.Common.Clause
import TestUtils.Limits
import HSat.Problem.BSP.Common.Clause
import Data.Word
import Data.Vector (Vector)

genClauses :: Gen (Vector Clause) -> Gen Word -> Gen Clauses
genClauses = liftM2 Clauses

genClausesValid :: (Word,Word) -> Gen Clause -> Gen Clauses
genClausesValid bounds genClause = do
  size <- choose bounds
  genClauses (V.replicateM (fromEnum size) genClause) (return size)

genClausesInvalid :: (Word,Word) -> Gen Clause -> Gen Clauses
genClausesInvalid bounds genClause = do
  size <- choose bounds
  offSet <- choose (1,maxBound)
  genClauses (V.replicateM (fromEnum size) genClause) (return $ size + offSet)

instance Arbitrary Clauses where
  arbitrary = genClausesValid (0,5) arbitrary
  shrink clauses =
    map mkClauses $ shrink . getVectClause $ clauses
