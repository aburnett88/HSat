module TestUtils.Problem.BSP.Common.Literal (
  mkArbLiteral,
  mkSmallLiteral
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Literal
import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Common.Sign
import TestUtils.Problem.BSP.Common.Sign
import TestUtils.Problem.BSP.Common.Variable

instance Arbitrary Literal where
  arbitrary = mkArbLiteral arbitrary arbitrary
  shrink (Literal s v) =
    map (uncurry mkLiteral) $ shrink (s,v)

mkArbLiteral :: Gen Sign -> Gen Variable -> Gen Literal
mkArbLiteral = liftM2 mkLiteral

mkSmallLiteral :: Gen Literal
mkSmallLiteral = mkArbLiteral arbitrary mkSmallVariable
