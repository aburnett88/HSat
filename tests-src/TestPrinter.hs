{-|
Module      : TestPrinter
Description : The Root of the Printing Tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The root of the Test Tree Node for the Printer tests.
-}

module Main (
  main -- :: IO ()
  ) where

import           TestPrint
import qualified TestPrint.Make as Make
import qualified TestPrint.Parser as Parser
import qualified TestPrint.Problem as Problem
import qualified TestPrint.Writer as Writer
import qualified TestPrint.Printer as Printer

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
    Parser.printer,
    Printer.printer
    ]
