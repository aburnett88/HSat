module Test.Data (
  tests
  ) where

import TestUtils
{-|
Module      : Test.Data
Description : The Data hierarchy testing module
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The tree node for all tests for modules with a suffix of Data
-}

import TestUtils
import qualified Test.Data.BSP as BSP

name :: String
name = "Test.Data"

tests :: TestTree
tests =
  testGroup name [
    BSP.tests
    ]
