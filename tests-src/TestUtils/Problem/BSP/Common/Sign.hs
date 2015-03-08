{-|
Module      : TestUtils.Problem.BSP.Common.Sign
Description : Generators for the Sign type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports generic functions for generating 'Sign's
-}

module TestUtils.Problem.BSP.Common.Sign (
  genSign -- :: Gen Sign
  ) where

import HSat.Problem.BSP.Common.Sign
import TestUtils.Test

{-|
Generates valid 'Sign's
-}
genSign :: Gen Sign
genSign = liftM mkSign arbitrary

instance Arbitrary Sign where
  arbitrary   = genSign
  shrink sign =
    map mkSign $ shrink . getBool $ sign
