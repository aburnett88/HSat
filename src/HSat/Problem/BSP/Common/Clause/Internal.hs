{-|
Module      : HSat.Problem.BSP.Common.Clause.Internal
Description : The Clause data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the definition of a 'Clause' which is in its simplest form
a list of 'Literal's.

These can be used, for example, to represent problems in Conjunctive Normal
Form and Disjunctive Normal Form, however are kept deliberatly abstract at
this stage.

We keep this module Internal to stop 'Clause' with incorrect 'getClauseLength'
values being created. 
-}

module HSat.Problem.BSP.Common.Clause.Internal (
  Clause(..)
  ) where

import Data.Word
import HSat.Printer
import qualified Data.Vector as V
import Data.Vector (Vector)
import HSat.Problem.BSP.Common.Literal

{-|
A 'Clause' describes a finite number of 'Literal's to be used as a generic
data structure for describing Boolean formulae.

Internally a 'Clause' is represented as a 'Vector' of 'Literal's and a cached
'Word' of its size. 
-}
data Clause = Clause {
  -- | The underlying 'Vector' of 'Literal's that is represented
  getVectLiteral   :: Vector Literal,
  -- | The cached size of the 'Clause' in 'Word' form
  getSizeClause :: Word
  } deriving (Eq,Show)

instance Printer Clause where
  compact   = printLit compact
  noUnicode = printLit noUnicode
  unicode   = printLit unicode

printLit                 :: (Literal -> Doc) -> Clause -> Doc
printLit function clause =
  encloseSep lbracket rbracket comma $
  map function . V.toList . getVectLiteral $ clause
