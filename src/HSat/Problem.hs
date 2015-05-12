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
  Problem,
  mkProblem     -- :: Source -> ProblemExpr -> Problem
  ) where

import HSat.Problem.Internal
import HSat.Problem.ProblemExpr
import HSat.Problem.Source

-- | Constructs a problem from the 'Source' and 'ProblemExpr'
mkProblem :: Source -> ProblemExpr -> Problem
mkProblem = Problem

