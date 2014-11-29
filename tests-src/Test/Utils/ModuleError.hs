{-|
Module      : Test.Utils.ModuleError
Description : The tests for the ModuleError type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This class represents any type of Boolean Satisfiability Problem (or will do once finished).

-}

module Test.Utils.ModuleError (
  tests
  ) where

import TestUtils
import HSat.Utils

name :: String
name = "HSat.Utils.ModuleError"

tests :: TestTree
tests =
  testGroup name [
    testProperty "Test correct return values" test1
    ]

test1         :: (String,String,String) -> Property
test1 (a,b,c) = property $ moduleErr a b c == (
  a ++ "." ++ b ++ ": "++c)
