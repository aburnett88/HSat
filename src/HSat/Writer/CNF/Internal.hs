module HSat.Writer.CNF.Internal (
  CNFWriter(..),
  WrittenProblemLine(..),
  WrittenClauseLine(..)
  ) where

import qualified Data.Vector as V
import HSat.Writer.Internal
import Data.Word
import HSat.Problem.BSP.Common

data CNFWriter = CNFWriter {
  writeProblemLine :: WrittenProblemLine,
  writeClauses :: V.Vector WrittenClauseLine
  } deriving (Eq,Show)

data WrittenProblemLine = WPL {
  wplVariables :: Word,
  wplClauses :: Word,
  wplComments :: [Comment]
  } deriving (Eq,Show)

data WrittenClauseLine = WCL {
  wpClause :: Clause,
  wclComments :: [Comment]
  } deriving (Eq,Show)
