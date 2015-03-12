{-|
Module      : TestUtils.Problem.BSP.Common.Literal
Description : Generators for the Literal type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports Generator functions for the Literal type
-}

module TestUtils.Problem.BSP.Common.Literal (
  genLiteral -- :: Word -> Gen Literal
  ) where

import Data.Word
import HSat.Problem.BSP.Common.Literal
import TestUtils.Problem.BSP.Common.Sign ()
import TestUtils.Problem.BSP.Common.Variable
import TestUtils.Test
import TestUtils.Validate

{-
Generate random sign, random variable (within context) then create a Literal from the parts.
-}
genLiteral        :: Word -> Gen Literal
genLiteral maxVar = do
  sign <- arbitrary
  var  <- genVariableContext maxVar
  return $ mkLiteral sign var

instance Arbitrary Literal where
  arbitrary            = genLiteral maxBound
  shrink (Literal s v) =
    map (uncurry mkLiteral) $ shrink (s,v)

instance Validate Literal where
  validate (Literal _ v) =
    validate v
