module Test.Parser.CNF (
  tests
  ) where

import TestUtils
import qualified Test.Parser.CNF.Internal as Internal

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests
    ]
