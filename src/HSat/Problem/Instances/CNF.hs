{-|
Module      : HSat.Problem.Instances.CNF
Description : The CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the Conjunctive Normal Form (CNF) data type and associated
functions
-}

module HSat.Problem.Instances.CNF (
  -- * CNF
  CNF(
     getMaxVar    ,
     getClauseNumb,
     getClauses
     ),
  mkCNFFromClauses ,  -- :: Clauses -> CNF
  -- * Integer Constructors
  cnfToIntegers    , -- :: CNF -> [[Integer]]
  mkCNFFromIntegers  -- :: [[Integer]] -> CNF
  ) where

import HSat.Problem.Instances.CNF.Internal
import HSat.Problem.Instances.CNF.Writer
import HSat.Problem.Instances.Common
import HSat.Problem.ProblemExpr.Class

{-|
Turns a 'Clauses' data type into a CNF data type
-}
mkCNFFromClauses    :: Clauses -> CNF
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

instance IsProblem CNF where
  getWriter p = Just ("cnf",toText p)
  fromCNF     = id
  toCNF       = id

  
