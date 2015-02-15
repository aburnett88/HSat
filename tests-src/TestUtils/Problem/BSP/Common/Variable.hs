module TestUtils.Problem.BSP.Common.Variable (
  mkArbVariable,
  mkSmallVariable
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Variable
import Data.Word
import TestUtils.Limits

mkArbVariable :: Gen Word -> Gen Variable
mkArbVariable = liftM mkVariable

instance Arbitrary Variable where
  arbitrary = mkArbVariable (choose (1,maxVariable))
  shrink v =
    map mkVariable $ filter (/=0) . shrink . getWord $ v

mkSmallVariable :: Gen Variable
mkSmallVariable = mkArbVariable (choose (1,100))
