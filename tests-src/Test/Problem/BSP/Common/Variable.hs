{-|
Module      : Test.Data.BSP.Common.Variable
Description : The Variable tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The test leaf for the Variable module
-}

module Test.Problem.BSP.Common.Variable (
  tests
  ) where

import qualified Control.Exception as E (catch)
import           Control.Exception.Base (ErrorCall)
import           Control.Monad (when)
import           Data.Maybe (isNothing)
import           Data.Word
import           HSat.Problem.BSP.Common.Variable
import           TestUtils

name :: String
name = "Variable"

tests :: TestTree
tests =
  testGroup name [
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
    word === (getWord . mkVariable $ word)
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
    abs int === (toInteger . getWord . mkVariableFromInteger $ int)
    )

mkVariableFromIntegerTest2 :: TestTree
mkVariableFromIntegerTest2 =
  testCase "mkVariableFromInteger 0 throws runtime error" $
  forceError (mkVariableFromInteger 0) (mkVariableFromInteger 1)

mkVariableFromIntegerTest3 :: TestTree
mkVariableFromIntegerTest3 =
  testCase "mkVariableFromInteger (maxBound + 1) throws runtime error" $
  forceError (
    mkVariableFromInteger . (1+) . toInteger $ (maxBound :: Word)
    ) (mkVariableFromInteger 1)

mkVariableFromIntegerTest4 :: TestTree
mkVariableFromIntegerTest4 =
  testCase "mkVarFromInteger (negate maxBound + 1) throws runtime error" $
  forceError (
    mkVariableFromInteger . negate . (1+) . toInteger $ (maxBound :: Word)
    ) (mkVariableFromInteger 1)

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
    abs int === (variableToInteger . mkVariableFromInteger $ int)
    )
