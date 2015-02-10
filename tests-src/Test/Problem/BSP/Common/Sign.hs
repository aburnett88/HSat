{-|
Module      : Test.Data.BSP.Common.Sign
Description : The Sign tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The test leaf for the Sign module
-}

module Test.Problem.BSP.Common.Sign (
  tests
  ) where

import qualified Control.Exception as E (catch)
import           Control.Exception.Base (ErrorCall)
import           Control.Monad (when)
import           Data.Maybe (isNothing)
import           HSat.Problem.BSP.Common.Sign
import           TestUtils

name :: String
name = "Sign"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkSign" [
       mkSignTest1
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
      isPosTest1
      ],
    testGroup "isNeg" [
      isNegTest1
      ]
    ]

mkSignTest1 :: TestTree
mkSignTest1 =
  testProperty "getBool . mkSign == id" $ property (
    \bool ->
    bool == (getBool . mkSign $ bool)
    )

posTest1 :: TestTree
posTest1 =
  testCase "mkSign True == pos" $ assert (
    pos == mkSign True
    )

negTest1 :: TestTree
negTest1 =
  testCase "mkSign False == neg" $ assert (
    neg == mkSign False
    )

mkSignFromIntegerTest1 :: TestTree
mkSignFromIntegerTest1 =
  testProperty "mkSignFromInteger nonZero == either pos or neg" $
  forAll
  mkIntegerNonZero
  (\int ->
    case compare int 0 of
      EQ -> False
      LT -> mkSignFromInteger int == neg
      GT -> mkSignFromInteger int == pos
  )

mkSignFromIntegerTest2 :: TestTree
mkSignFromIntegerTest2 =
  testProperty "compare 0 . signToInteger . mkSignFromInteger $ int == compare 0 int" $
  forAll
  mkIntegerNonZero
  (\int ->
    compare 0 (signToInteger . mkSignFromInteger $ int) == compare 0 int
  )

mkSignFromIntegerTest3 :: TestTree
mkSignFromIntegerTest3 =
  testCase "mkSignFromInteger 0 == error" $ assert $ do
    maybValue <- E.catch (
      do
        --A cheeky way of forcing evaluation so the error is called
        let ans = Just . mkSignFromInteger $ 0
            dummyVal = Just pos
        when (dummyVal==ans) (print ans) >> return ans
      ) ((\_ -> return Nothing) :: ErrorCall -> IO (Maybe Sign))
    assert . isNothing $ maybValue

signToIntegerTest1 :: TestTree
signToIntegerTest1 =
  testCase "signToInteger (mkSign True) == 1" $ assert (
    (signToInteger . mkSign $ True) == 1
    )

signToIntegerTest2 :: TestTree
signToIntegerTest2 =
  testCase "signToInteger (mkSign False) == -1" $ assert (
    (signToInteger . mkSign $ False) == -1
    )

signToIntegerTest3 :: TestTree
signToIntegerTest3 =
  testProperty "mkSignFromInteger . signToInteger == id" $ property (
    \sign ->
    (mkSignFromInteger . signToInteger $ sign) == sign
    )

isPosTest1 :: TestTree
isPosTest1 =
  testProperty "isPos . mkSign $ x == x" $ property (
    \bool ->
    (isPos . mkSign $ bool) == bool
    )

isNegTest1 :: TestTree
isNegTest1 =
  testProperty "isNeg . mkSign $ x == (not x)" $ property (
    \bool ->
    (isNeg . mkSign $ bool) == not bool
    )
