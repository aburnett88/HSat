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
  CNF |
  -- | Describes a SAT problem that is just a Boolean Formula in no special form
  BSP
  deriving (Eq,Show)

cnfDoc :: Doc
cnfDoc = text "CNF"

bspDoc :: Doc
bspDoc = text "BSP"

instance Printer ProblemType where
  compact CNF = cnfDoc
  compact BSP = bspDoc
  noUnicode CNF = cnfDoc
  noUnicode BSP = bspDoc
  unicode CNF = cnfDoc
  unicode BSP = bspDoc
  
