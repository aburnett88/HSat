{-|
Module      : Test
Description : The root of the Test Tree
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The root of the TestTree, and the only available point of entry
-}

module Main (
  main
  ) where

import           TestUtils
import qualified Test.Problem as Problem
import qualified Test.Make as Make
import qualified Test.Writer as Writer
import qualified Test.Parser as Parser

main :: IO ()
main = defaultMain tests

name :: String
name = "HSat Tests"

tests :: TestTree
tests =
  testGroup name [
    Problem.tests,
    Make.tests,
    Writer.tests,
    Parser.tests
    ]
