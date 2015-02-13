module Test.Parser (
  tests,
  printer
  ) where

import TestUtils
import qualified Test.Parser.CNF as CNF

name :: String
name = "Parser"

tests :: TestTree
tests =
  testGroup name [
    CNF.tests
    ]

printer :: TestTree
printer =
  testGroup name []
