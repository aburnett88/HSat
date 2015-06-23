{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{- |
Module      : HSat.Problem.Internal
Description : Internal representation of Problem
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The internal representation of a 'Problem' that contains a source
and a representation
-}

module HSat.Problem.Internal (
  Problem(..)
  ) where

import HSat.Printer
import HSat.Problem.ProblemExpr.Class
import HSat.Problem.Source

{-|
A 'Problem' represents both a problem expression and its source
-}
data Problem where
  MkProblem :: {
    source      :: Source     ,
    problemExpr :: ProblemExpr
    } -> Problem

instance Eq Problem where
  (MkProblem s p) == (MkProblem s' p') = s == s' && p == p'
  
instance Show Problem where
  showsPrec = show'

instance Printer Problem where
  compact   = printProblem Compact
  noUnicode = printProblem NoUnicode
  unicode   = printProblem Unicode

printProblem                     :: PrinterType -> Problem -> Doc
printProblem pType MkProblem{..} =
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

