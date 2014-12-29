{-|
Module      : Test.Data.BSP.Common.Literal
Description : Tests for hte Literal data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The tests for the Literal type
-}
module Test.Data.BSP.Common.Literal (
  tests
  ) where

import TestUtils
import HSat.Data.BSP.Common.Literal (Literal)
import qualified HSat.Data.BSP.Common.Literal as L
import qualified HSat.Data.BSP.Common.Sign as S (toBool)
import qualified HSat.Data.BSP.Common.Variable as V (getWord)

name :: String
name = "Test.Data.BSP.Common.Literal"

tests :: TestTree
tests = testGroup name [
  testProperty "fromInt . toInt == id" test1,
  testProperty "fromTuple . toTuple == id" test2,
  testProperty "fromBool, fromWord are correct" test3,
  testCase "isNegative -12 == True" test4,
  testCase "isNegative 12 == False" test5,
  testCase "isPositive -12 == False" test6,
  testCase "isPositive 12 == True" test7,
  testCase "litInRange 100 -99 == True" test8,
  testCase "litInRange 88 -99 == False" test9,
  testCase "fromInt' 0 . toInt == 0" test10
  ]

test1   :: Literal -> Property
test1 l = property $ l == (L.fromInt . L.toInt $ l)

test2   :: Literal -> Property
test2 l = property $ l == (L.fromTuple . L.toTuple $ l)

test3                   :: Literal -> Property
test3 l@(L.Literal s w) = property $ words && bools
  where
    words = V.getWord w == L.getWord l
    bools = S.toBool s == L.getBool l

test4 :: Assertion
test4 =
  assert $
  (L.isNegative . L.fromInt $ -12)

test5 :: Assertion
test5 =
  assert $
  (not . L.isNegative . L.fromInt $ 12)

test6 :: Assertion
test6 =
  assert $
  (not . L.isPositive . L.fromInt $ -12)

test7 :: Assertion
test7 =
  assert $
  (L.isPositive . L.fromInt $ 12)

test8 :: Assertion
test8 =
  assert $
  (L.litInRange 100 $ L.fromInt (-99))

test9 :: Assertion
test9 =
  assert $
  (not $ L.litInRange 88 $ L.fromInt (-99))

test10 :: Assertion
test10 =
  assert $
  ((L.toInt . L.fromInt' $ 0) == 0)
