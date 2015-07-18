{-|
Module      : Test.Parser.Class
Description : The Parser Class tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the tests for the Class module
-}

module Test.Parser.Class (
  tests -- :: TestTree
  ) where

import TestUtils

name :: String
name = "Class"

tests :: TestTree
tests =
  testGroup name []
