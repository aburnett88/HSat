module Test.Make.BSP (
  tests
  ) where

import TestUtils
import qualified Test.Make.BSP.Common as Common
import qualified Test.Make.BSP.CNF as CNF

name :: String
name = "BSP"

tests :: TestTree
tests =
  testGroup name [
    Common.tests,
    CNF.tests
    ]
