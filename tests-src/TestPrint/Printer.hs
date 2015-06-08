module TestPrint.Printer (
  printer
  ) where

import TestPrint
import HSat.Printer

name :: String
name = "Printer"

printer :: TestTree
printer =
  testGroup name [
    printerTypes
    ]

printerTypes :: TestTree
printerTypes =
  printList "PrinterTest" [Compact,Unicode,NoUnicode]
