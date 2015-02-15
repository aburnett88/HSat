
module TestPrint.Writer (
  printer
  ) where

import TestUtils

name :: String
name = "Writer"

printer :: TestTree
printer =
  testGroup name []
