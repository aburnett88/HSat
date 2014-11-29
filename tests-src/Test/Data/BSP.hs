{-|
Module      : Test.Data.BSP
Description : The tests for the BSP type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

All tests for the BSP type and any modules below this
level in the hierarchy structure. 
-}

module Test.Data.BSP (
  tests
  ) where

import TestUtils
import qualified Test.Data.BSP.CNF as CNF
import qualified Test.Data.BSP.Common as Common
import qualified HSat.Data.BSP as BSP

name :: String
name = "HSat.Data.BSP"

tests :: TestTree
tests =
  testGroup name [
    Common.tests,
    CNF.tests
    ]
