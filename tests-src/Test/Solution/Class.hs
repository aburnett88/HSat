{-|
Module      : Test.Solution.Class
Description : Tests the Solution Class module and its sub-modules
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Solution Class modules
-}

module Test.Solution (
  tests,-- :: TestTree
  ) where

import TestUtils

name :: String
name = "Class"

tests :: TestTree
tests =
  testGroup name [
    ]
