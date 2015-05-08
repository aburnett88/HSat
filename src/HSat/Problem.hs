{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

{- |
Module      : HSat.Problem
Description : The Problem data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

A 'Problem' consists of the problem representation and the source of the
problem.
-}

module HSat.Problem (
  -- * Problem
  Problem (..), -- :: Problem
  mkProblem     --  :: Source -> ProblemExpr -> Problem
  ) where

import           HSat.Printer
import           HSat.Problem.ProblemExpr
import           HSat.Problem.Source

{- |
The data type describing a 'Problem'
-}
data Problem = Problem {
  -- | The problem source
  source      :: Source     ,
  -- | The problem representation  showsPrec = show'
  problemExpr :: ProblemExpr
  } deriving (Eq)

-- | Constructs a problem from the 'Source' and 'ProblemExpr'
mkProblem :: Source -> ProblemExpr -> Problem
mkProblem = Problem

-- Printer instances

instance Printer Problem where
  compact   = printProblem Compact
  noUnicode = printProblem NoUnicode
  unicode   = printProblem Unicode

printProblem                   :: PrinterType -> Problem -> Doc
printProblem pType Problem{..} =
  preamble    <>
  space'      <>
  printSource <>
  line        <>
  printExpr
  where
    preamble :: Doc
    preamble = case pType of
      Compact -> "Problem -"
      _       -> "Problem:"
    space' :: Doc
    space' = case pType of
      Compact -> space
      _       -> line
    printSource :: Doc
    printSource = pTypeToDoc pType source
    printExpr   :: Doc
    printExpr   = pTypeToDoc pType problemExpr
