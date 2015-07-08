{-|
Module      : Test.Problem.Source.Internal
Description : The Internal Source tests
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Exports all high level tests and internal module tests for the Source type
-}

module Test.Problem.Source.Internal (
  tests -- TestTree
  ) where

import TestUtils

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name []
