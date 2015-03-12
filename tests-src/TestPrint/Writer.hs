
module TestPrint.Writer (
  printer
  ) where

import TestPrint

name :: String
name = "Writer"

printer :: TestTree
printer =
  testGroup name []
