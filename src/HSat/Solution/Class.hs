{-# LANGUAGE
    MultiParamTypeClasses ,
    FunctionalDependencies,
    TypeFamilies
    #-}

{-|
Module      : HSat.Solution.Class
Description : Exports the 'Solution' type class
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports functionality for the 'Solution' type class
-}

module HSat.Solution.Class (
  Solution(..)
  ) where

import HSat.Problem.ProblemExpr.Class

{-|
A solution has a functional dependency of what types of solution are associated with a type
-}
class (IsProblem problem) => Solution problem where
  type SolInstance problem
  checkSolution :: problem -> SolInstance problem -> Bool
