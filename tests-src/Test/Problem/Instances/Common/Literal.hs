{-|
Module      : Test.Problen.Instances.Common.Literal
Description : The Literal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the Test Tree Node for the Literal module, as well as associated
Generator functions
-}

module Test.Problem.Instances.Common.Literal (
  tests,     -- :: TestTree
  genLiteral -- :: Word -> Gen Literal 
  ) where

import Control.Applicative
import HSat.Problem.Instances.Common.Literal
import Test.Problem.Instances.Common.Sign     ()
import Test.Problem.Instances.Common.Variable (genVariableContext)
import TestUtils
import TestUtils.Validate

name :: String
name = "Literal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkLiteral" [
       mkLiteralTest1
       ],
    testGroup "mkLiteralFromInteger" [
      mkLiteralFromIntTest1
      ],
    testGroup "literalToInteger" [
      literalToIntegerTest1
      ]
    ]

mkLiteralTest1 :: TestTree
mkLiteralTest1 =
  testProperty ("getVariable " `equiv` " v, getSign " `equiv` " s in mkLiteral s v") $ property
  (\(sign,variable) ->
    let valLiteral = mkLiteral sign variable
        valSign    = getSign valLiteral
        valVar     = getVariable valLiteral
    in (valSign === sign) .&&.
       (valVar  === variable)
  )

mkLiteralFromIntTest1 :: TestTree
mkLiteralFromIntTest1 =
  testProperty ("literalToInteger . mkLiteralFromInteger " `equiv` " id") $
  forAll
  mkIntegerNonZero
  (\int ->
    let val = literalToInteger $ mkLiteralFromInteger int
    in val === int
  )

literalToIntegerTest1 :: TestTree
literalToIntegerTest1 =
  testProperty ("mkLiteralFromInteger . literalToInteger " `equiv` " id") $ property
  (\lit ->
    let val = mkLiteralFromInteger $ literalToInteger lit
    in lit === val
  )

genLiteral        :: Word -> Gen Literal
genLiteral maxVar =
  liftA2 mkLiteral arbitrary (genVariableContext maxVar)

instance Arbitrary Literal where
  arbitrary            = genLiteral maxBound
  shrink (Literal s v) =
    map (uncurry mkLiteral) $ shrink (s,v)

instance Validate Literal where
  validate (Literal _ v) =
    validate v
