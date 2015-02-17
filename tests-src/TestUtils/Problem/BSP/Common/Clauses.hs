module TestUtils.Problem.BSP.Common.Clauses (
  genClauses
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

genClauses :: Word -> Word -> Word -> Gen Clauses
genClauses sizeBound sizeClause maxVar = do
  size <- choose (0,sizeBound)
  vector <- V.replicateM (fromEnum size) (genClause sizeClause maxVar)
  return $ mkClauses vector

instance Arbitrary Clauses where
  arbitrary = genClauses 5 5 100
  shrink clauses =
    map mkClauses $ shrink . getVectClause $ clauses
