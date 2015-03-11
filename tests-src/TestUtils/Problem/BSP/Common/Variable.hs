{-|
Module      : TestUtils.Problem.BSP.Common.Variable
Description : Generators for the Variable type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports Generator functions for the Variable type
-}

module TestUtils.Problem.BSP.Common.Variable (
  genVariable,       -- :: Gen Variable
  genVariableContext -- :: Word -> Gen Variable
  ) where

import Data.Word
import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Common.Variable.Internal
import TestUtils.Limits
import TestUtils.Test
import TestUtils.Validate

genVariable :: Gen Variable
genVariable = genVariableContext maxBound

genVariableContext     :: Word -> Gen Variable
genVariableContext max = liftM mkVariable $ choose (1,max) 

instance Arbitrary Variable where
  arbitrary = genVariable
  shrink v  =
    map mkVariable $ filter (/=0) . shrink . getWord $ v

{-
Only a Variable with a value of zero is an invalid Variable
-}
instance Validate Variable where
  validate (Variable 0) = False
  validate _            = True
