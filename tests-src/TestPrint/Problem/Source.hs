{-|
Module      : TestPrint.Problen.Source
Description : The Source type Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the Source type's Printer Tests
-}

module TestPrint.Problem.Source (
  printer, -- :: TestTree
  sources  -- :: [Source]
  ) where

import HSat.Problem.Source
import TestPrint

name :: String
name = "Source"

printer :: TestTree
printer =
  testGroup name [
    printSources
    ]

printSources :: TestTree
printSources =
  printList "Sources" sources

sources :: [Source]
sources = [
    mkStatic,
    mkFileSource "/Hello/World/inter.cnf"
    ]
