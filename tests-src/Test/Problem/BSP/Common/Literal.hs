{-|
Module      : Test.Problen.BSP.Common.Literal
Description : The Literal tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Literal module
-}

module Test.Problem.BSP.Common.Literal (
  tests
  ) where

import HSat.Problem.BSP.Common.Literal
import TestUtils

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
  testProperty "getVariable == v, getSign == s in mkLiteral s v" $ property
  (\(sign,variable) ->
    let valLiteral = mkLiteral sign variable
        valSign    = getSign valLiteral
        valVar     = getVariable valLiteral
    in (valSign === sign) .&&.
       (valVar  === variable)
  )

mkLiteralFromIntTest1 :: TestTree
mkLiteralFromIntTest1 =
  testProperty "literalToInteger . mkLiteralFromInteger == id" $
  forAll
  mkIntegerNonZero
  (\int ->
    let val = literalToInteger $ mkLiteralFromInteger int
    in val === int
  )

literalToIntegerTest1 :: TestTree
literalToIntegerTest1 =
  testProperty "mkLiteralFromInteger . literalToInteger == id" $ property
  (\lit ->
    let val = mkLiteralFromInteger $ literalToInteger lit
    in lit === val
  )
