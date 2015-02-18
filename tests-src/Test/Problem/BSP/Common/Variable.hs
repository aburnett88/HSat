{-|
Module      : Test.Data.BSP.Common.Variable
Description : The Variable tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Variable module
-}

module Test.Problem.BSP.Common.Variable (
  tests
  ) where

import           Data.Word
import           HSat.Problem.BSP.Common.Variable
import qualified Test.Problem.BSP.Common.Variable.Internal as Internal
import           TestUtils

name :: String
name = "Variable"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    testGroup "mkVariable" [
       mkVariableTest1,
       mkVariableTest2
       ],
    testGroup "mkVariableFromInteger" [
      mkVariableFromIntegerTest1,
      mkVariableFromIntegerTest2,
      mkVariableFromIntegerTest3,
      mkVariableFromIntegerTest4
      ],
    testGroup "varInRange" [
      varInRangeTest1
      ],
    testGroup "variableToInteger" [
      variableToIntegerTest1
      ]
    ]

mkVariableTest1 :: TestTree
mkVariableTest1 =
  testProperty "getWord . mkVariable == id" $
  forAll
  mkWordNonZero
  (\word ->
    let gottenVal = getWord $ mkVariable word
    in word       === gottenVal
       )

mkVariableTest2 :: TestTree
mkVariableTest2 =
  testCase "mkVariale 0 throws runtime error" $
  forceError (mkVariable 0) (mkVariable 1)

mkVariableFromIntegerTest1 :: TestTree
mkVariableFromIntegerTest1 =
  testProperty "fromEnum . getWord . mkVariableFromInteger == id" $
  forAll
  mkIntegerNonZero
  (\int ->
    let expectedVal = abs int
        gottenVal   = toInteger . getWord . mkVariableFromInteger $ int
    in expectedVal  === gottenVal
       )

mkVariableFromIntegerTest2 :: TestTree
mkVariableFromIntegerTest2 =
  testCase "mkVariableFromInteger 0 throws runtime error" $
  forceError (mkVariableFromInteger 0) (mkVariableFromInteger 1)

mkVariableFromIntegerTest3 :: TestTree
mkVariableFromIntegerTest3 =
  testCase "mkVariableFromInteger (maxBound + 1) throws runtime error" $ do
    let throwErrorVal = mkVariableFromInteger . (1+) . toInteger $ (
          maxBound :: Word)
        dummyVal      = mkVariableFromInteger 1
    forceError throwErrorVal dummyVal

mkVariableFromIntegerTest4 :: TestTree
mkVariableFromIntegerTest4 =
  testCase "mkVarFromInteger (negate maxBound + 1) throws runtime error" $ do
    let throwErrorVal = mkVariableFromInteger . negate . (1+) . toInteger $ (
          maxBound :: Word)
        dummyVal      = mkVariableFromInteger 1
    forceError throwErrorVal dummyVal

varInRangeTest1 :: TestTree
varInRangeTest1 =
  testProperty "Returned result is consistant with input" $ property
  (\(range,var) ->
    case compare (getWord var) range of
      GT -> varInRange range var === False
      _  -> varInRange range var === True
      )
  

variableToIntegerTest1 :: TestTree
variableToIntegerTest1 =
  testProperty "variableToInteger . mkVariableFromInteger == abs" $
  forAll
  mkIntegerNonZero
  (\int ->
    let expectedVal = abs int
        gottenVal   = variableToInteger $ mkVariableFromInteger int
    in expectedVal  === gottenVal
       )
