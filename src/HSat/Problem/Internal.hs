{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

{- |
Module      : HSat.Problem.Internal
Description : Internal representation of Problem
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The internal represetnation of a 'Problem' that contains a souce
and a representation
-}

module HSat.Problem.Internal (
  Problem(..)
  ) where

import HSat.Printer
import HSat.Problem.ProblemExpr
import HSat.Problem.Source

{-|
A 'Problem' represents both a problem expresion and its source
-}
data Problem = Problem {
  -- | The Source of the Problem
  source      :: Source     ,
  -- | The expressino of the problem
  problemExpr :: ProblemExpr
  } deriving (Eq)

instance Show Problem where
  showsPrec = show'

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
