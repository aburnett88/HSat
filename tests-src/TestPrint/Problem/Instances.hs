module TestPrint.Problem.Instances (
  printer
  ) where

import TestPrint

name :: String
name = "Instances"

printer :: TestTree
printer =
  testGroup name []
