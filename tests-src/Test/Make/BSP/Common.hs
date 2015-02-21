module Test.Make.BSP.Common (
  tests
  ) where

import TestUtils
import qualified Test.Make.BSP.Common.Literal as Literal
import qualified Test.Make.BSP.Common.Clause as Clause
import qualified Test.Make.BSP.Common.Clauses as Clauses

name :: String
name = "Common"

tests :: TestTree
tests =
  testGroup name [
    Literal.tests,
    Clause.tests,
    Clauses.tests
    ]
