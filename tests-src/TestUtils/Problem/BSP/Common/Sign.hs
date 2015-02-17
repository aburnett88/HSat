{-|
Module      : TestUtils.Problem.BSP.Common.Sign
Description : Gen types for Signs
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports generic functions for generating 'Sign's
-}

module TestUtils.Problem.BSP.Common.Sign (
  genSign, -- :: Gen Sign
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Sign

{-|
Generates a valid 'Sign'
-}
genSign :: Gen Sign
genSign = liftM mkSign arbitrary

instance Arbitrary Sign where
  arbitrary = genSign
  shrink sign =
    map mkSign $ shrink . getBool $ sign
