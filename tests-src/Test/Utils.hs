{-|
Module      : Test.Utils
Description : The Utils tests
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Contains the tests for the Utils hierarchy of modules
-}

module Test.Utils (
  tests
  ) where

import TestUtils
-- import Test.Utils.Printer
import qualified Test.Utils.ModuleError as ModuleError

name :: String
name = "HSat.Utils"

tests :: TestTree
tests =
  testGroup name [
    ModuleError.tests
    ]
