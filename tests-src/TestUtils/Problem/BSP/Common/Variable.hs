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
  genVariable, -- :: Gen Word -> Gen Variable
  genVariableValid, -- :: Gen Variable
  genVariableValidContext, -- Word -> Gen Variable
  genVariableInvalidContext, -- :: Word -> Gen Variable
  genVariableInvalid -- :: Gen Variable
  ) where

import Data.Word
import HSat.Problem.BSP.Common.Variable.Internal
import TestUtils.Limits
import TestUtils.Test

{-|
Generates a 'Variable' from a 'Word' generator
-}
genVariable :: Gen Word -> Gen Variable
genVariable = liftM Variable

{-|
Generates a 'Valid' 'Variable'
-}
genVariableValid :: Gen Variable
genVariableValid = genVariable $ choose (1,maxBound)

{-|
Generates an invalid 'Variabl'e
-}
genVariableInvalid :: Gen Variable
genVariableInvalid = genVariable $ return 0

{-|
Generates a valid 'Variable' in a context
-}
genVariableValidContext :: Word -> Gen Variable
genVariableValidContext 0 = error ("impossible")
genVariableValidContext maxVar = genVariable $ choose (1,maxVar)

genVariableInvalidContext :: Word -> Gen Variable
genVariableInvalidContext n =
  oneof [genVariableInvalid,
         genVariable $ choose (n+1,maxBound)
         ]

instance Arbitrary Variable where
  arbitrary = genVariableValid
  shrink v =
    map Variable $ filter (/=0) . shrink . getWord $ v
