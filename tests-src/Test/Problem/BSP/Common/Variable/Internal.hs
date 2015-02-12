{-|
Module      : Test.Problen.BSP.Common.Variable.Internal
Description : The 'Variable' Internal Test Leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Variable Internal module
-}

module Test.Problem.BSP.Common.Variable.Internal (
  tests
  ) where

import TestUtils
import HSat.Problem.BSP.Common.Variable.Internal

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
  testProperty "valiate variable == True" $ property
  (\variable ->
    validate variable
    )

variableTest2 :: TestTree
variableTest2 =
  testCase "validate (Variable 0) == False" $
  assertBool "Validation should not pass" (not $ validate (Variable 0))
