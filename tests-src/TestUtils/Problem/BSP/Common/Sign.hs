module TestUtils.Problem.BSP.Common.Sign (
  mkPosSign,
  mkNegSign,
  mkArbSign
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Sign

mkArbSign :: Gen Bool -> Gen Sign
mkArbSign = liftM mkSign

mkPosSign :: Gen Sign
mkPosSign = mkArbSign (return True)

mkNegSign :: Gen Sign
mkNegSign = mkArbSign (return False)

instance Arbitrary Sign where
  arbitrary = mkArbSign arbitrary
  shrink sign =
    map mkSign $ shrink . getBool $ sign
