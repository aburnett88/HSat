{-|
Module      : Test.Data.BSP.Common.Sign
Description : The Sign data type tests
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The tests for the Sign type
-}
module Test.Data.BSP.Common.Sign (
  tests
  ) where

import TestUtils
import HSat.Data.BSP.Common.Sign (Sign)
import qualified HSat.Data.BSP.Common.Sign as S

name :: String
name = "HSat.Data.BSP.Common.Sign"

tests :: TestTree
tests = testGroup name [
  testProperty "fromBool" test1,
  testProperty "fromInt" test2,
  testProperty "x == toBool . fromBool $ x" test3,
  testCase "isPos var" test4,
  testCase "isNeg varNeg" test5,
  testCase "fromInt'" test6
  ]

test1       :: Bool -> Property
test1 True  = property $ S.var == S.fromBool True
test1 False = property $ S.varNeg == S.fromBool False

test2 :: Int -> Property
test2 i
  | i == 0 = property $ True
  | i < 0  = property $ S.fromInt i == S.varNeg
  | otherwise = property $ S.fromInt i == S.var

test3   :: Bool -> Property
test3 b = property $ b == (S.toBool . S.fromBool $ b)

test4 :: Assertion
test4 = assert $ S.isPos S.var

test5 :: Assertion
test5 = assert $ S.isNeg S.varNeg

test6 :: Assertion
test6 =
  assert $
  (S.fromInt' 0 == S.var)
