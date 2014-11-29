{-|
Module      : Test.Data.BSP.Common.Variable
Description : Tests for the Variable datatype
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

the tests for the variable type
-}
module Test.Data.BSP.Common.Variable (
  tests
  ) where

import TestUtils
import HSat.Data.BSP.Common.Variable (Variable)
import qualified HSat.Data.BSP.Common.Variable as V

name :: String
name = "Test.Data.BSP.Common.Variable"

tests :: TestTree
tests = testGroup name [
  testProperty "fromWord . getWord == id" test1,
  testProperty "getword . fromInt == fromEnum" test2,
  testCase "getWord . fromWord' 0 == 0" test3,
  testCase "varInRange 10 (Variable 6)" test4,
  testCase "varInRange 4 (Variable 6)" test5
  ]

test1   :: Variable -> Property
test1 v = property $ v == (V.fromWord . V.getWord $ v)

test2   :: Variable -> Property
test2 v = property $ (v) == (V.fromInt . fromEnum . V.getWord $ v)

test3 :: Assertion
test3 =
  assert $
  ( (V.getWord . V.fromWord' $ 0) == 0)

test4 :: Assertion
test4 =
  assert $
  ( (V.varInRange 10 (V.fromWord 6)) == True)

test5 :: Assertion
test5 =
  assert $
  (V.varInRange 6 (V.fromWord 10) == False)

