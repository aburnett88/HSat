module TestPrint.Problem.BSP.Common (
  printer
  ) where

import qualified TestPrint.Problem.BSP.Common.Clause as Clause
import qualified TestPrint.Problem.BSP.Common.Clauses as Clauses
import qualified TestPrint.Problem.BSP.Common.Literal as Literal
import qualified TestPrint.Problem.BSP.Common.Sign as Sign
import qualified TestPrint.Problem.BSP.Common.Variable as Variable
import           TestUtils

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
