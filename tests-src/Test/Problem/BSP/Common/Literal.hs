{-|
Module      : Test.Problen.BSP.Common.Literal
Description : The 'Literal' test leaf
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The TestTree Leaf for the Literal module
-}

module Test.Problem.BSP.Common.Literal (
  tests,
  printer
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

printer :: TestTree
printer =
  testGroup name [
    printLiteralArbitrary
    ]

printLiteralArbitrary :: TestTree
printLiteralArbitrary =
  printTest "Literal" (
    (generate arbitrary) :: IO Literal)

mkLiteralTest1 :: TestTree
mkLiteralTest1 =
  testProperty "getVariable == v, getSign == s in mkLiteral s v" $ property
  (\(sign,variable) ->
    let resSign = getSign $ mkLiteral sign variable
        resVar  = getVariable $ mkLiteral sign variable
    in (resSign === sign) .&&. (resVar === variable)
  )

mkLiteralFromIntTest1 :: TestTree
mkLiteralFromIntTest1 =
  testProperty "literalToInteger . mkLiteralFromInteger == id" $
  forAll
  mkIntegerNonZero
  (\int ->
    literalToInteger (mkLiteralFromInteger int) === int
    )

literalToIntegerTest1 :: TestTree
literalToIntegerTest1 =
  testProperty "mkLiteralFromInteger . literalToInteger == id" $ property
  (\lit ->
    mkLiteralFromInteger (literalToInteger lit) === lit
    )
