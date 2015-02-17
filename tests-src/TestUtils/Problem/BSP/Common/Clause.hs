module TestUtils.Problem.BSP.Common.Clause (
  genClause, -- :: Word -> Word -> Gen Clause
  genClauseFixedSize -- :: Word -> Word -> Gen Clause
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Clause.Internal
import HSat.Problem.BSP.Common.Clause
import HSat.Problem.BSP.Common.Literal
import qualified Data.Vector as V (replicateM,empty)
import TestUtils.Problem.BSP.Common.Literal
import TestUtils.Limits
import Data.Word
import Data.Vector (Vector)

genClauseFixedSize :: Word -> Word -> Gen Clause
genClauseFixedSize sizeBond 0 = error "genClauseFixedSize 0"
genClauseFixedSize sizeBound maxVar = do
  vector <- V.replicateM (fromEnum sizeBound) (genLiteral maxVar)
  return $ mkClause vector

genClause :: Word -> Word -> Gen Clause
genClause sizeBound 0 = do
  return $ mkClause V.empty
genClause sizeBound maxVar = do
  size <- choose (0,sizeBound)
  genClauseFixedSize size maxVar

instance Arbitrary Clause where
  arbitrary = genClause 5 100
  shrink cl =
    map mkClause $ shrink . getVectLiteral $ cl
