module TestPrint.Make (
  printer
  ) where

import TestPrint

name :: String
name = "Make"

printer :: TestTree
printer = testGroup name []
