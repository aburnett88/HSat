{-|
Module      : TestPrinter
Description : The printing node of the TestTree
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The root of the TestTree for Printer, and the only available point of entry
-}

module Main (
  main
  ) where

import TestUtils
import qualified Test.Problem as Problem
import qualified Test.Make as Make
import qualified Test.Writer as Writer
import qualified Test.Parser as Parser

main :: IO ()
main = defaultMain printer

name :: String
name = "HSat Printer Tests"

printer :: TestTree
printer =
  testGroup name [
    Problem.printer,
    Make.printer,
    Writer.printer,
    Parser.printer
    ]
