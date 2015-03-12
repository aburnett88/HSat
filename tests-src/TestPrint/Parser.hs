module TestPrint.Parser (
  printer
  ) where

import TestPrint

name :: String
name = "Parser"

printer :: TestTree
printer =
  testGroup name []
