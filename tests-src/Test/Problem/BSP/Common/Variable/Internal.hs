{-|
Module      : Test.Problen.BSP.Common.Variable.Internal
Description : The Variable internal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the internal Variable module
-}

module Test.Problem.BSP.Common.Variable.Internal (
  tests
  ) where

import HSat.Problem.BSP.Common.Variable.Internal
import TestUtils
import TestUtils.Validate

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    variableTest1,
    variableTest2
    ]

variableTest1 :: TestTree
variableTest1 =
  testProperty "validate variable == True" $ property testVariable
  where
    testVariable :: Variable -> Bool
    testVariable = validate

{-
A variable with a value of zero should not be valid
-}
variableTest2 :: TestTree
variableTest2 =
  testCase "validate (Variable 0) == False" $
  assertBool "Validation should " (not . validate $ Variable 0)
