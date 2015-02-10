{-|
Module      : Test.Problen.BSP.Common.Literal
Description : The 'Literal' test leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The tests for the 'Literal' data type
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
       mkLiteralTest1,
       mkLiteralTest2
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
  testProperty "getVariable . mkLiteral s v == v" $ property (
    \(sign,var) ->
     getVariable (mkLiteral sign var) == var
    )

mkLiteralTest2 :: TestTree
mkLiteralTest2 =
  testProperty "getSign . mkLiteral s v == s" $ property (
    \(sign,var) ->
     getSign (mkLiteral sign var) == sign
    )

mkLiteralFromIntTest1 :: TestTree
mkLiteralFromIntTest1 =
  testProperty "literalToInteger . mkLiteralFromInteger == id" $ property (
    forAll
    mkIntegerNonZero
    (\int ->
      (literalToInteger . mkLiteralFromInteger $ int) == int
      )
    )

literalToIntegerTest1 :: TestTree
literalToIntegerTest1 =
  testProperty "mkLiteralFromInteger . literalToInteger lit = lit" $ property (
    \lit ->
    (mkLiteralFromInteger . literalToInteger $ lit) == lit
    )
