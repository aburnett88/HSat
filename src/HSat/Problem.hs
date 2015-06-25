{- |
Module      : HSat.Problem
Description : The Problem data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports definitions of the 'Problem' data type
-}

module HSat.Problem (
  -- * Problem
  Problem(MkProblem)
--  MkProblem     -- :: Source -> ProblemExpr -> Problem
  ) where

import HSat.Problem.Internal
