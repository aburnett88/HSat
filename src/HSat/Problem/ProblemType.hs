{-|
Module      : HSat.Problem.ProblemType
Description : The ProblemType data type
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Provides a unary constructor for describing a 'Problem' at an abstract level
-}

module HSat.Problem.ProblemType (
  -- * ProblemType
  ProblemType(..)
  ) where

import HSat.Printer

{-|
The general type describing for describing Problems
-}
data ProblemType =
  -- | Describes a SAT problem in Conjunctive Normal Form
  CNF
  deriving (Eq,Show)

cnfDoc :: Doc
cnfDoc = text "CNF"

instance Printer ProblemType where
  compact CNF = cnfDoc
  noUnicode CNF = cnfDoc
  unicode CNF = cnfDoc
