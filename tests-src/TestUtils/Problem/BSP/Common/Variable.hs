{-|
Module      : TestUtils.Problem.BSP.Common.Variable
Description : Gen types for Variables
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports generic functions for generating 'Variable's
-}

module TestUtils.Problem.BSP.Common.Variable (
  genVariable, -- :: Gen Variable
  genVariableContext -- :: Word -> Gen Variable
  ) where

import Data.Word
import HSat.Problem.BSP.Common.Variable.Internal
import HSat.Problem.BSP.Common.Variable
import TestUtils.Limits
import TestUtils.Test
import TestUtils.Validate

genVariable :: Gen Variable
genVariable = liftM mkVariable $ choose (1,maxBound)

genVariableContext :: Word -> Gen Variable
genVariableContext max = liftM mkVariable $ choose (1,max) 

instance Arbitrary Variable where
  arbitrary = genVariable
  shrink v =
    map mkVariable $ filter (/=0) . shrink . getWord $ v

instance Validate Variable where
  validate (Variable 0) = False
  validate _            = True
