
module Test.Writer (
  tests
  ) where

import TestUtils
import qualified Test.Writer.CNF as CNF
import qualified Test.Writer.Internal as Internal

name :: String
name = "Writer"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests,
    CNF.tests
    ]
