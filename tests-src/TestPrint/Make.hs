module TestPrint.Make (
  printer
  ) where

import TestUtils

name :: String
name = "Make"

printer :: TestTree
printer = testGroup name []
