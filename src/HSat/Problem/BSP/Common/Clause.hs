{-|
Module      : HSat.Problem.BSP.Common.Clause
Description : The Clause data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the Clause data type and associated functions.

A Clause consists of a abstract list of 'Literal's, which in turn represent either the positive or negative occurence of a 'HSat.Data.BSP.Common.Variable'
-}

module HSat.Problem.BSP.Common.Clause (
  -- * Data Type
  Clause(getLiterals,clauseLength),
  -- * Construction
  mkClause,
  mkClauseFromLits,
  mkClauseFromIntegers,
  clauseAddLit,
  clauseToIntegers,
  clauseIsEmpty
  ) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word
import           HSat.Printer
import           HSat.Problem.BSP.Common.Literal

{-|
A Clause contains a finite number of Literals. Internally
this is represented as a Vector
-}
data Clause = Clause {
  -- | The vector of Literals that represents the Clause
  getLiterals :: Vector Literal,
  -- | The size of the 'Clause'
  clauseLength :: Word
  } deriving (Eq,Show)

{-|
Creates an empty 'Clause'
-}
mkClause :: Clause
mkClause = Clause V.empty 0

{-|
creates a 'Clause' from a list of 'Literal's
-}
mkClauseFromLits :: [Literal] -> Clause
mkClauseFromLits  =
  foldl clauseAddLit mkClause

{-|
Takes a 'Clause' and a 'Literal' as arguments and appends
the 'Literal' to the 'Clause'
-}
clauseAddLit                :: Clause -> Literal -> Clause
clauseAddLit (Clause c n) l = Clause (V.snoc c l) (n+1)

{-|
Converts a list of 'Literal's to a 'Clause'
-}
mkClauseFromIntegers :: [Integer] -> Clause
mkClauseFromIntegers =
  foldl (\clause int -> clauseAddLit clause $ mkLiteralFromInteger int) mkClause
{-|
Turns a 'Clause' into a list of 'Literal's
-}
clauseToIntegers              :: Clause -> [Integer]
clauseToIntegers (Clause c _) =
  V.toList . V.map literalToInteger $ c

{-|
Checks whether a 'Clause' is empty
-}
clauseIsEmpty               :: Clause -> Bool
clauseIsEmpty (Clause cl _) = cl == V.empty

--Printer instance

instance Printer Clause where
  compact   = printLit compact
  noUnicode = printLit noUnicode
  unicode   = printLit unicode

printLit     :: (Literal -> Doc) -> Clause -> Doc
printLit f c =
  encloseSep lbracket rbracket comma $ map f . V.toList . getLiterals $ c
