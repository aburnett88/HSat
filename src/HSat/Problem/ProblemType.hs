{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HSat.Problem.ProblemType
Description : The ProblemType data type
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Provides a data type to describe the class of a 'Problem'
-}

module HSat.Problem.ProblemType (
  -- * ProblemType
  ProblemType(..)
  ) where

import HSat.Printer

{-|
The data type to describe a 'Problem'
-}
data ProblemType =
  -- | Describes a SAT problem in Conjunctive Normal Form
  CNF |
  -- | Describes a SAT problem that is just a Boolean Formula in no special form
  BSP
  deriving (Eq,Show)

cnfDoc,bspDoc :: Doc
cnfDoc = "CNF"
bspDoc = "BSP"

instance Printer ProblemType where
  compact   CNF = cnfDoc
  compact   BSP = bspDoc
  noUnicode CNF = cnfDoc
  noUnicode BSP = bspDoc
  unicode   CNF = cnfDoc
  unicode   BSP = bspDoc
  
