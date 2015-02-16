module TestUtils.Problem.BSP.Common.Clause (
  genClause, -- :: Gen (Vector Literal) -> Word -> Gen Clause
  genClauseValid, -- :: (Int,Int) -> Gen Literal -> Gen Clause
  genClauseInvalid
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Clause.Internal
import HSat.Problem.BSP.Common.Clause
import HSat.Problem.BSP.Common.Literal
import qualified Data.Vector as V (replicateM)
import TestUtils.Problem.BSP.Common.Literal
import TestUtils.Limits
import Data.Word
import Data.Vector (Vector)

genClause :: Gen (Vector Literal) -> Gen Word -> Gen Clause
genClause = liftM2 Clause

genClauseValid :: (Word,Word) -> Gen Literal -> Gen Clause
genClauseValid bounds genLit = do
  size <- choose bounds
  genClause (V.replicateM (fromEnum size) genLit) (return size)

genClauseInvalid :: (Word,Word) -> Gen Literal -> Gen Clause
genClauseInvalid bounds genLit = do
  size <- choose bounds
  offSet <- choose (1,maxBound)
  genClause (V.replicateM (fromEnum size) genLit) (return (size + offSet))

instance Arbitrary Clause where
  arbitrary = genClauseValid (0,5) arbitrary
  shrink cl =
    map mkClause $ shrink . getVectLiteral $ cl
