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
  genSign, -- :: Gen Bool -> Gen Sign
  genSignValid -- :: Gen Sign
  ) where

import TestUtils.Test
import HSat.Problem.BSP.Common.Sign

{-|
Creates a 'Sign' from a generator for 'Bool'
-}
genSign :: Gen Bool -> Gen Sign
genSign = liftM mkSign

{-|
Generates a valid 'Sign'
-}
genSignValid :: Gen Sign
genSignValid = genSign arbitrary

instance Arbitrary Sign where
  arbitrary = genSignValid
  shrink sign =
    map mkSign $ shrink . getBool $ sign
