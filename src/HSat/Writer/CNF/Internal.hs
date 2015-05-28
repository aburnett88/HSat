{-|
Module      : HSat.Writer.CNF.Internal
Description : Exports the data types for the CNFWriter
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports the data types for the CNFWriter, should anything manual need to be done
-}

module HSat.Writer.CNF.Internal (
  CNFWriter(..),
  WrittenProblemLine(..),
  WrittenClauseLine(..)
  ) where

import qualified Data.Vector as V
import HSat.Writer.Internal
import HSat.Problem.BSP.Common

{-|
The CNFWriter is the supertype containing all the parts of the writer
-}
data CNFWriter = CNFWriter {
  -- | The preamble, or 'WrittenProblemLine'
  writeProblemLine :: WrittenProblemLine,
  -- | A vector of 'WrittenClauseLine's
  writeClauses :: V.Vector WrittenClauseLine
  } deriving (Eq,Show)

{-|
'WrittenProblemLine' contains the number of 'Clauses', 'Variables' and assocated 'Comment's
-}
data WrittenProblemLine = WPL {
  -- | The number of 'Variable's
  wplVariables :: Word,
  -- | The number of 'Clauses'
  wplClauses :: Word,
  -- | A list of 'Comment's for the preamble
  wplComments :: [Comment]
  } deriving (Eq,Show)

{-|
The WrittenClauseLine contains a 'Clause' and the comments associated with it
-}
data WrittenClauseLine = WCL {
  -- | The 'Clause' itself
  wpClause :: Clause,
  -- | A list of 'Comment's for the 'Clause'
  wclComments :: [Comment]
  } deriving (Eq,Show)
