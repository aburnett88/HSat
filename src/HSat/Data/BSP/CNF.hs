{-|
Module      : HSat.Data.BSP.CNF
Description : The CNF data type
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the Conjunctive Normal Form (CNF) data type and associated functions
-}
module HSat.Data.BSP.CNF (
  CNF(..),
  fromClauses,
  toClauses,
  toInts,
  fromInts

  ) where

import Data.Word
import HSat.Data.BSP.Common.Clauses (Clauses)
import HSat.Data.BSP.Common.Clauses as CL
import qualified HSat.Data.BSP.Common.Clause as C
import qualified HSat.Data.BSP.Common.Literal as L

{-|
The Conjunctive Normal Form type. It is much like a 'Clauses'
type but with additional information.
-}
data CNF = CNF {
  -- | The range of 'Variable's within the CNF Problem
  _noVars :: Word,
  -- | The number of 'Clause's in the problem
  _noClauses :: Int,
  -- | The problems 'Clauses' themselves
  _getClauses :: Clauses
  } deriving (Eq,Show)

{-|
Turns a Clauses data type into a CNF datatype
-}
fromClauses :: Clauses -> CNF
fromClauses cl =
  let v = CL.maxVariable cl
      n = CL.length cl
  in CNF v n cl

{-|
Turns a CNF data type to a Clauses data type
-}
toClauses :: CNF -> Clauses
toClauses = _getClauses

{-|
Takes a 'CNF' data structure and converts it into a list of 'Int's
-}
toInts :: CNF -> [[Int]]
toInts xs =
  map (map L.toInt . C.toList) . CL.toList . _getClauses $ xs

{-|
Converts a list of 'Int's into a CNF data structure
-}
fromInts :: [[Int]] -> CNF
fromInts xs = fromClauses . CL.fromList . map (C.fromList . map L.fromInt) $ xs
