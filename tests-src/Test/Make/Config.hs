{-|
Module      : Test.Make.Config
Description : Tests the Make Config file
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the test hierarchy for the Config modules
-}

module Test.Make.Config (
  tests -- TestTree
  ) where

import qualified Test.Make.Config.Class as Class
import           TestUtils

name :: String
name = "Config"

tests :: TestTree
tests =
  testGroup name [
    Class.tests
    ]
