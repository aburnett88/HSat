{-|
Module      : Test.Data.Instances.Common.Sign
Description : The Sign tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the Test Tree Node for the Sign module, as well as associated
Generator functions
-}

module Test.Problem.Instances.Common.Sign (
  tests
  ) where

import HSat.Problem.Instances.Common.Sign
import TestUtils
import Control.Applicative

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
      mkSignFromIntegerTest3,
      mkSignFromIntegerTest4
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
  testCase "getBool . mkSign True == True" $
  getBool (mkSign True) @=? True

mkSignTest2 :: TestTree
mkSignTest2 =
  testCase "getBool . mkSign False == False" $
  False @=? getBool (mkSign False)
  
posTest1 :: TestTree
posTest1 =
  testCase "mkSign True == pos" $
  pos @=? mkSign True

negTest1 :: TestTree
negTest1 =
  testCase "mkSign False == neg" $
  neg @=? mkSign False

{-
positive integers should all create positive signs
-}
mkSignFromIntegerTest1 :: TestTree
mkSignFromIntegerTest1 =
  testProperty "mkSignFromInteger posInteger == pos" $
  forAll
  mkPosIntegerNonZero
  (\int -> mkSignFromInteger int === pos)

--negative integers should create negative signs
mkSignFromIntegerTest2 :: TestTree
mkSignFromIntegerTest2 =
  testProperty "mkSignFromInteger negInteger == neg" $
  forAll
  mkNegIntegerNonZero
  (\int -> mkSignFromInteger int === neg)

{-
Comparing the input and comparing something from and to a Sign should preserve
its comparison to zero
-}
mkSignFromIntegerTest3 :: TestTree
mkSignFromIntegerTest3 =
  testProperty "signToInteger mkSignFromInteger has correct sign" $
  forAll
  mkIntegerNonZero
  (\int ->
    let exptd = compare 0 int
        val   = compare 0 (signToInteger $ mkSignFromInteger int)
    in exptd === val
  )

mkSignFromIntegerTest4 :: TestTree
mkSignFromIntegerTest4 =
  testCase "mkSignFromInteger 0 throws Error" $
  forceError $ mkSignFromInteger 0

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
    let val = mkSignFromInteger $ signToInteger sign
    in sign === val
  )

isPosTest1 :: TestTree
isPosTest1 =
  testCase "isPos $ mkSign True == True" $
  True @=? isPos (mkSign True)

isPosTest2 :: TestTree
isPosTest2 =
  testCase "isPos $ mkSign False == False" $
  False @=? isPos (mkSign False)

isNegTest1 :: TestTree
isNegTest1 =
  testCase "isNeg $ mkSign True == False" $
  False @=? isNeg (mkSign True)

isNegTest2 :: TestTree
isNegTest2 =
  testCase "isNeg $ mkSign False == True" $
  True @=? isNeg (mkSign False)

{-
Generators and Arbitrary instances
-}
genSign :: Gen Sign
genSign = liftA mkSign arbitrary

instance Arbitrary Sign where
  arbitrary   = genSign
  shrink sign =
    map mkSign $ shrink . getBool $ sign
