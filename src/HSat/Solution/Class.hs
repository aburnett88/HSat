{-# LANGUAGE
   MultiParamTypeClasses,
   FunctionalDependencies
   #-}

module HSat.Solution.Class (
  Solution(..)
  ) where

import HSat.Problem.ProblemExpr.Class

class (IsProblem problem) => Solution problem solution | problem -> solution where
  checkSolution :: problem -> solution -> Bool
