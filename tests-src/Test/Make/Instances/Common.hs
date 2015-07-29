{-|
Module      : Test.Make.Instances.Common
Description : Tests the Common instances for Make
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Make Common modules
-}

module Test.Make.Instances.Common (
  tests, -- :: TestTree
  ) where

import TestUtils
import qualified Test.Make.Instances.Common.Literal as Literal
import qualified Test.Make.Instances.Common.Clause as Clause

name :: String
name = "Common"

tests :: TestTree
tests =
  testGroup name [
    Literal.tests,
    Clause.tests
    ]
