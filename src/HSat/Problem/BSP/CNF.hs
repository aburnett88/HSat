{-|
Module      : HSat.Problem.BSP.CNF
Description : The CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the Conjunctive Normal Form (CNF) data type and associated
functions
-}

module HSat.Problem.BSP.CNF (
  -- * CNF
  CNF(getNoVars,getNoClauses,getClauses),
  mkCNFFromClauses,
  -- * Integer Constructors
  cnfToIntegers,
  mkCNFFromIntegers,
  validate
  ) where

import qualified Data.Vector as V
import           Data.Word
import           HSat.Printer
import           HSat.Problem.BSP.Common
import HSat.Problem.BSP.CNF.Internal

{-|
Turns a 'Clauses' data type into a CNF datatype
-}
mkCNFFromClauses :: Clauses -> CNF
mkCNFFromClauses cl =
  let v = findMaxVar cl
      c = getSizeClauses cl
  in CNF v c cl
     
{-|
Takes a 'CNF' data structure and converts it into a list of 'Integer's
-}
cnfToIntegers :: CNF -> [[Integer]]
cnfToIntegers = clausesToIntegers . getClauses

{-|
Converts a list of 'Integer's into a CNF data structure
-}
mkCNFFromIntegers :: [[Integer]] -> CNF
mkCNFFromIntegers = mkCNFFromClauses . mkClausesFromIntegers


validate :: CNF -> Bool
validate cnf =
  let v = findMaxVar $ getClauses cnf
      c = getSizeClauses $ getClauses cnf
  in getNoVars cnf == v &&
     getNoClauses cnf == c

  
