{-|
Module      : HSat.Problen.BSP.Common.Clauses.Internal
Description : The 'Clause' data type definition
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the definition of 'Clauses', a data structure used to
hold a collection of 'Clause'
-}

module HSat.Problem.Instances.Common.Clauses.Internal (
  Clauses(..),
  printClausesWithContext
  ) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           HSat.Printer
import           HSat.Problem.Instances.Common.Clause
import           HSat.Problem.Instances.Common.Clause.Internal (
  printClauseWithContext)
import           HSat.Problem.Instances.Common.Literal

{-|
A 'Clauses' represents a list of 'Clause', which in themselves represent
'Literal's in a problem.

This general data structure can be thought of as representing a Boolean formula
in Conjuctive or Disjunctive Normal Form.

Internally represented as a 'Vector' of 'Clause' and a cached 'Word' that
represents the size of the 'Vector'
-}
data Clauses = Clauses {
  -- | Internally represented as a 'Vector' of 'Clause'
  getVectClause  :: Vector Clause,
  -- | A size variable describing how many 'Clause' are contained
  getSizeClauses :: Word
  } deriving (Eq)

instance Show Clauses where
  showsPrec = show'

instance Printer Clauses where
  compact   = generalPrinter compact
  noUnicode = generalPrinter noUnicode
  unicode   = generalPrinter unicode

generalPrinter :: (Clause -> Doc) -> Clauses -> Doc
generalPrinter func clauses =
  encloseSep lbracket rbracket comma (
    map func . V.toList $ getVectClause clauses)

{-|
Takes a string that separates clauses, a string that separates clauses, a word
denoting the maximum variable, a literal to doc function, and prints these all
-}
printClausesWithContext :: String -> String -> Word -> (Literal -> Doc) ->
                           Clauses -> Doc
printClausesWithContext sepClauses sepClause maxVar function clauses =
  vsep $ punctuate (text sepClauses) clausesDoc
  where
    clausesDoc :: [Doc]
    clausesDoc =
      V.toList . V.map (printClauseWithContext sepClause maxVar function) $
                 getVectClause clauses
