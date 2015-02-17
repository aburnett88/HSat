{-|
Module      : HSat.Problem
Description : The general Problem type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The general 'Problem' type describes all types of 'Problem', there 'Source' and
accompanying 'ProblemExpr'
-}

module HSat.Problem (
  -- Problem
  Problem(..), -- :: Problem
  mkProblem    -- :: Source -> ProblemExpr -> Problem
  ) where

import HSat.Printer
import HSat.Problem.ProblemExpr
import HSat.Problem.Source

{-|
The general 'Problem' type describes all the types of 'ProblemExpr' and provides
an accompanying 'Source'
-}
data Problem = Problem {
  -- | The 'Source' of the 'Problem'
  getSource :: Source          ,
  -- | The expression 'ProblemExpr' of the problem itself
  getProblemExpr :: ProblemExpr
  } deriving (Eq)

instance Show Problem where
  showsPrec = show'

{-|
Constructs a problem from the 'Source' and 'ProblemExpr' parts
-}
mkProblem :: Source -> ProblemExpr -> Problem
mkProblem = Problem

-- Printer instance

instance Printer Problem where
  compact = printProblem Compact
  noUnicode = printProblem NoUnicode
  unicode = printProblem Unicode

printProblem :: PrinterType -> Problem -> Doc
printProblem pType (Problem source expr) =
  preamble <>
  space' <>
  printSource <>
  line <>
  printExpr
  where
    preamble :: Doc
    preamble = text $ case pType of
      Compact -> "Problem -"
      _ -> "Problem:"
    space' :: Doc
    space' = case pType of
      Compact -> space
      _ -> line
    printSource :: Doc
    printSource = pTypeToDoc pType source
    printExpr :: Doc
    printExpr = pTypeToDoc pType expr
