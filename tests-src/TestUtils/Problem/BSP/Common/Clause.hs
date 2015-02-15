module TestUtils.Problem.BSP.Common.Clause (
  mkArbClause,
  mkSmallClause
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Clause
import HSat.Problem.BSP.Common.Literal
import qualified Data.Vector as V (replicateM)
import TestUtils.Problem.BSP.Common.Literal
import TestUtils.Limits

mkArbClause :: Gen Literal -> Int -> Gen Clause
mkArbClause genLit sizeBound = do
  size <- choose (0,sizeBound)
  liftM mkClause (V.replicateM size genLit)

instance Arbitrary Clause where
  arbitrary = mkArbClause arbitrary maxClause

mkSmallClause :: Gen Clause
mkSmallClause = mkArbClause mkSmallLiteral 5
