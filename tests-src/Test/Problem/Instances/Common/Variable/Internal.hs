{-|
Module      : Test.Problen.Instances.Common.Variable.Internal
Description : The Variable internal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the internal Variable module
-}

module Test.Problem.Instances.Common.Variable.Internal (
  tests             , -- TestTree
  genVariableContext  -- Word -> Gen Variable
  ) where

import HSat.Problem.Instances.Common.Variable.Internal
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
  testProperty ("validate variable" `equiv` " True") $ property testVariable
  where
    testVariable :: Variable -> Bool
    testVariable = validate

{-
A variable with a value of zero should not be valid
-}
variableTest2 :: TestTree
variableTest2 =
  testCase ("validate (Variable 0)" `equiv` "False") $
  assertBool "Validation should " (not . validate $ Variable 0)

genVariable :: Gen Variable
genVariable = genVariableContext maxBound

genVariableContext         :: Word -> Gen Variable
genVariableContext maxWord = Variable <$> choose (1,maxWord) 

instance Arbitrary Variable where
  arbitrary = genVariable
  shrink v  =
    map Variable $ filter (/=0) . shrink . getWord $ v

{-
Only a Variable with a value of zero is an invalid Variable
-}
instance Validate Variable where
  validate (Variable 0) = False
  validate _            = True
