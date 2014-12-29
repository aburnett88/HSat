{-|
Module      : HSat.Data.BSP
Description : Boolean Satisfiability type (BSP)
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This class represents any type of Boolean Satisfiability Problem (or will do once finished).
-}

module HSat.Data.BSP (
  -- * Data Types
  BSP(..),
  Source(..),
  Problem(..)
  ) where

import HSat.Data.BSP.CNF

{-|
A BSP type represents a Boolean Formula and consists of two parts; a source and a problem representation. 
-}
data BSP = BSP {
  -- | The 'Source' of the problem
  _source  :: Source,
  -- | The 'Problem' itself
  _problem :: Problem
  } deriving (Eq,Show)

{-|
A 'Source' represents where the problem came from
-}
data Source =
  -- | A static source; written in code and cannot be changed
  StatisSource
  deriving (Eq,Show)

{-|
A 'Problem' represents the problem itself, and how it is represented. 
-}
data Problem =
  -- | A Problem in Conjunctive Normal Form ('CNF'). 
  ProblemCNF CNF
  deriving (Eq,Show)
