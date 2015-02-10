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
  } deriving (Eq,Show)

{-|
Constructs a problem from the 'Source' and 'ProblemExpr' parts
-}
mkProblem :: Source -> ProblemExpr -> Problem
mkProblem = Problem

-- Printer instance

instance Printer Problem where
  compact (Problem s p) =
    text ("Problem -") <+>
    compact s <>
    line <>
    compact p
  noUnicode             = printVerbose noUnicode noUnicode
  unicode               = printVerbose unicode unicode

printVerbose                   :: (Source -> Doc) ->
                                  (ProblemExpr -> Doc) ->
                                  Problem ->
                                  Doc
printVerbose f g (Problem s p) =
  text ("Problem:") <>
  indent 2 (f s) <>
  line <>
  indent 2 ( g p)
