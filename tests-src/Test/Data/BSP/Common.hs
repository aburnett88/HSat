{-|
Module      : Test.Data.BSP.Common
Description : The tests for the Common module
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The tests for the common module and at all points in the
hierarchy under this
-}

module Test.Data.BSP.Common (
  tests
  ) where

import TestUtils
import qualified Test.Data.BSP.Common.Clause as Clause
import qualified Test.Data.BSP.Common.Clauses as Clauses
import qualified Test.Data.BSP.Common.Literal as Literal
import qualified Test.Data.BSP.Common.Sign as Sign
import qualified Test.Data.BSP.Common.Variable as Variable


name :: String
name = "HSat.Data.BSP.Common"

tests :: TestTree
tests =
  testGroup name [
    Clause.tests,
    Clauses.tests,
    Literal.tests,
    Sign.tests,
    Variable.tests
    ]
