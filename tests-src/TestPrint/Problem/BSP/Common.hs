{-|
Module      : TestPrint.Problen.BSP.Common
Description : The Common module node for the Printer tests
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The Test Tree Node for the BSP's Common type's Printer Tests
-}

module TestPrint.Problem.BSP.Common (
  printer -- :: TestTree
  ) where

import qualified TestPrint.Problem.BSP.Common.Clause as Clause
import qualified TestPrint.Problem.BSP.Common.Clauses as Clauses
import qualified TestPrint.Problem.BSP.Common.Literal as Literal
import qualified TestPrint.Problem.BSP.Common.Sign as Sign
import qualified TestPrint.Problem.BSP.Common.Variable as Variable
import           TestPrint

name :: String
name = "Common"

printer :: TestTree
printer =
  testGroup name [
    Clauses.printer,
    Clause.printer,
    Literal.printer,
    Variable.printer,
    Sign.printer
    ]
