{-# LANGUAGE
    OverloadedStrings
    #-}

{-|
Module      : Test.Printer
Description : The Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the tests for the Printer type.
-}

module Test.Printer (
  tests -- :: TestTree
  ) where

import HSat.Printer
import TestUtils

name :: String
name = "Printer"

tests :: TestTree
tests =
  testGroup name [
    testGroup "errorDoc" [
       errorDocTest1
       ]
    ]

instance Arbitrary PrinterType where
  arbitrary =
    oneof $ map return [Compact,NoUnicode,Unicode]

instance Arbitrary Doc where
  arbitrary = fmap text arbitrary

errorDocTest1 :: TestTree
errorDocTest1 =
  testProperty "errorDoc prepends correct string" $ property
  (\(printerType,doc) ->
    let exptd = case printerType of
          Compact   -> "ERR" <> colon <+> doc
          NoUnicode -> "Error" <> colon <+> doc
          Unicode   -> red $ "Error" <> colon <+> doc
        gotten = errorDoc printerType doc
    in show exptd ===  show gotten --need to show as no Eq constraint
  )
