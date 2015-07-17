{-|
Module      : Test.Problem.Instances.Common.Variable
Description : The Variable tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Variable module
-}

module Test.Problem.Instances.Common.Variable (
  tests                      , -- TestTree
  Internal.genVariableContext  -- Word -> Gen Variable
  ) where

import           HSat.Problem.Instances.Common.Variable
import qualified Test.Problem.Instances.Common.Variable.Internal as Internal
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
  testProperty ("getWord . mkVariable" `equiv` "id") $
  forAll
  mkWordNonZero
  (\word ->
    let val = getWord $ mkVariable word
    in word === val
  )
  
mkVariableTest2 :: TestTree
mkVariableTest2 =
  testCase "mkVariale 0 throws run-time error" $
  forceError $ mkVariable 0

mkVariableFromIntegerTest1 :: TestTree
mkVariableFromIntegerTest1 =
  testProperty ("getWord . mkVariableFromInteger" `equiv` "id") $
  forAll
  mkIntegerNonZero
  (\int ->
    let exptd = abs int
        val   = toInteger . getWord .
                mkVariableFromInteger $ int
    in exptd === val
  )

mkVariableFromIntegerTest2 :: TestTree
mkVariableFromIntegerTest2 =
  testCase "mkVariableFromInteger 0 throws error" $
  forceError $ mkVariableFromInteger 0

mkVariableFromIntegerTest3 :: TestTree
mkVariableFromIntegerTest3 =
  testCase "mkVariableFromInteger (maxBound + 1) throws error" $
  let input = mkVariableFromInteger . (1+) .
              toInteger $ (maxBound :: Word)
  in forceError input

mkVariableFromIntegerTest4 :: TestTree
mkVariableFromIntegerTest4 =
  testCase "mkVarFromInteger (negate maxBound + 1) throws error" $
  let input = mkVariableFromInteger . negate .
              (1+) . toInteger $ (maxBound :: Word)
  in forceError input

{-
only if a variable is outside the range is it invalid. Zero is a valid variable
for now, however this test will not catch this fact
-}
varInRangeTest1 :: TestTree
varInRangeTest1 =
  testProperty "Returned result is consistent with input" $ property
  (\(range, var) ->
    case compare (getWord var) range of
      GT -> varInRange range var === False
      _  -> varInRange range var === True
  )

variableToIntegerTest1 :: TestTree
variableToIntegerTest1 =
  testProperty ("variableToInteger . mkVariableFromInteger" `equiv` "abs") $
  forAll
  mkIntegerNonZero
  (\int ->
    let exptd = abs int
        val   = variableToInteger $ mkVariableFromInteger int
    in exptd === val
  )
