{-|
Module      : Test.Data.BSP.Common.Clause
Description : The tests for the Clause data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

-}
module Test.Data.BSP.Common.Clause (
  tests
  ) where

import TestUtils
import HSat.Data.BSP.Common.Clause (Clause)
import HSat.Data.BSP.Common.Clause as C
import Data.Vector as V (length,fromList)
import HSat.Data.BSP.Common.Literal as L (fromInt,getWord)

name :: String
name = "Test.Data.BSP.Common.Clause"

tests :: TestTree
tests = testGroup name [
  testProperty "fromList . toList == id" test1,
  testProperty "foldl addLit empty == id" test2,
  testProperty "length == V.length" test3,
  testCase "checkLitBounds 100 (gen 101) == False" test4,
  testCase "checkLitBounds 100 (gen 100) == True" test5,
  testProperty "maxVariable == maximum" test6,
  testCase "isEmpty [] == True" test7
  ]

test1   :: Clause -> Property
test1 c = property $ c == (C.fromList . C.toList $ c)

test2   :: Clause -> Property
test2 c = property $ c == c'
  where
    c' = foldl C.addLit C.empty $ C.toList c

test3   :: Clause -> Property
test3 c = property $
          (toEnum . V.length . C._getLits $ c) == C.length c

test4 :: Assertion
test4 =
  assert $
  not $ C.checkLitBounds 100 (
    Clause . V.fromList . map L.fromInt $ (
       (map negate [1..101]) ++ [1..101]))

test5 :: Assertion
test5 =
  assert $
  C.checkLitBounds 100 (
    Clause . V.fromList . map L.fromInt $ (
       (map negate [1..100]) ++ [1..100]))

test6   :: Clause -> Property
test6 c = property $ C.maxVariable c == (
  if C.isEmpty c then 0 else
    (maximum . map L.getWord . C.toList $ c))

test7 :: Assertion
test7 =
  assert $
    C.isEmpty (C.empty) == True
