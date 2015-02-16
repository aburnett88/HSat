module TestUtils.Problem.BSP.Common.Literal (
  genLiteral, -- :: Gen Sign -> Gen Bool -> Gen Literal
  genLiteralValid -- :: Gen Literal
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Literal
import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Common.Sign
import TestUtils.Problem.BSP.Common.Sign
import TestUtils.Problem.BSP.Common.Variable

genLiteral :: Gen Sign -> Gen Variable -> Gen Literal
genLiteral genSign genLiteral = liftM2 mkLiteral genSign genLiteral

genLiteralValid :: Gen Literal
genLiteralValid = genLiteral genSignValid genVariableValid

instance Arbitrary Literal where
  arbitrary = genLiteralValid
  shrink (Literal s v) =
    map (uncurry mkLiteral) $ shrink (s,v)
