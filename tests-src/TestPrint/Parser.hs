module TestPrint.Parser (
  printer
  ) where

import TestUtils

name :: String
name = "Parser"

printer :: TestTree
printer =
  testGroup name []
