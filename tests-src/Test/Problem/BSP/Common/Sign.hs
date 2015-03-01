{-|
Module      : Test.Data.BSP.Common.Sign
Description : The Sign tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Sign module
-}

module Test.Problem.BSP.Common.Sign (
  tests
  ) where

import HSat.Problem.BSP.Common.Sign
import TestUtils

name :: String
name = "Sign"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkSign" [
       mkSignTest1,
       mkSignTest2
       ],
    testGroup "pos" [
      posTest1
      ],
    testGroup "neg" [
      negTest1
      ],
    testGroup "mkSignFromInteger" [
      mkSignFromIntegerTest1,
      mkSignFromIntegerTest2,
      mkSignFromIntegerTest3
      ],
    testGroup "signToInteger" [
      signToIntegerTest1,
      signToIntegerTest2,
      signToIntegerTest3
      ],
    testGroup "isPos" [
      isPosTest1,
      isPosTest2
      ],
    testGroup "isNeg" [
      isNegTest1,
      isNegTest2
      ]
    ]

mkSignTest1 :: TestTree
mkSignTest1 =
  testCase "getBool . mkSign == id" $
  (getBool $ mkSign True) @=? True

mkSignTest2 :: TestTree
mkSignTest2 =
  testCase "getBool . mkSign False == False" $
  False @=? (getBool $ mkSign False)
  
posTest1 :: TestTree
posTest1 =
  testCase "mkSign True == pos" $
  pos @=? mkSign True

negTest1 :: TestTree
negTest1 =
  testCase "mkSign False == neg" $
  neg @=? mkSign False

mkSignFromIntegerTest1 :: TestTree
mkSignFromIntegerTest1 =
  testProperty "mkSignFromInteger nonZero has correct sign" $
  forAll
  mkIntegerNonZero
  (\int ->
    case compare int 0 of
      EQ -> counterexample
            ("compare " ++ show int ++ " 0 === EQ") False
      LT -> mkSignFromInteger int === neg
      GT -> mkSignFromInteger int === pos
  )

mkSignFromIntegerTest2 :: TestTree
mkSignFromIntegerTest2 =
  testProperty "signToInteger mkSignFromInteger has correct sign" $
  forAll
  mkIntegerNonZero
  (\int ->
    let exptd  = compare 0 int
        actual = compare 0 (signToInteger $ mkSignFromInteger int)
    in exptd  === actual
  )

mkSignFromIntegerTest3 :: TestTree
mkSignFromIntegerTest3 =
  testCase "mkSignFromInteger 0 throws Error" $
  assert $ forceError (mkSignFromInteger 0) pos

signToIntegerTest1 :: TestTree
signToIntegerTest1 =
  testCase "signToInteger (mkSign True) == 1" $
  1 @=? signToInteger (mkSign True)

signToIntegerTest2 :: TestTree
signToIntegerTest2 =
  testCase "signToInteger (mkSign False) == -1" $
  (-1) @=? signToInteger (mkSign False)

signToIntegerTest3 :: TestTree
signToIntegerTest3 =
  testProperty "mkSignFromInteger . signToInteger == id" $ property
  (\sign ->
    let actual = mkSignFromInteger $ signToInteger sign
    in sign    === actual
  )

isPosTest1 :: TestTree
isPosTest1 =
  testCase "isPos $ mkSign True == True" $
  True @=? (isPos $ mkSign True)

isPosTest2 :: TestTree
isPosTest2 =
  testCase "isPos $ mkSign False == False" $
  False @=? (isPos $ mkSign False)

isNegTest1 :: TestTree
isNegTest1 =
  testCase "isNeg $ mkSign True == False" $
  False @=? (isNeg $ mkSign True)

isNegTest2 :: TestTree
isNegTest2 =
  testCase "isNeg $ mkSign False == True" $
  True @=? (isNeg $ mkSign False)
