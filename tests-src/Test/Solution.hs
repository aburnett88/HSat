{-|
Module      : Test.Solution
Description : Tests the Solution module and its sub-modules
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Solution modules
-}

module Test.Solution (
  tests,-- :: TestTree
  ) where

import qualified Test.Solution.Class     as Class
import qualified Test.Solution.Instances as Instances
import TestUtils

name :: String
name = "Solution"

tests :: TestTree
tests =
  testGroup name [
    Class.tests    ,
    Instances.tests
    ]
