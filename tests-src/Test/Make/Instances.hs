{-|
Module      : Test.Make.Instances
Description : Tests the Instances Make modules
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Make Instances modules
-}

module Test.Make.Instances (
  tests -- TestTree
  ) where

import qualified Test.Make.Instances.CNF    as CNF
import qualified Test.Make.Instances.Common as Common
import           TestUtils

name :: String
name = "Instances"

tests :: TestTree
tests =
  testGroup name [
    CNF.tests,
    Common.tests
    ]
