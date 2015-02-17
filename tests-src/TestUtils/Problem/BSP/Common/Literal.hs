module TestUtils.Problem.BSP.Common.Literal (
  genLiteral -- :: Word -> Gen Literal
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Literal
import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Common.Sign
import TestUtils.Problem.BSP.Common.Sign
import TestUtils.Problem.BSP.Common.Variable
import Data.Word

genLiteral :: Word -> Gen Literal
genLiteral max = do
  sign <- arbitrary
  var  <- genVariableContext max
  return $ mkLiteral sign var

instance Arbitrary Literal where
  arbitrary = genLiteral maxBound
  shrink (Literal s v) =
    map (uncurry mkLiteral) $ shrink (s,v)
