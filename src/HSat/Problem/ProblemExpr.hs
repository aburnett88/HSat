{-# LANGUAGE 
  MultiParamTypeClasses, 
  FunctionalDependencies #-}

{-|
Module      : HSat.Problem.ProblemExpr
Description : The ProblemExpr type and its associated functions
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Exports the general definition of all 'ProblemExpr's defined
-}

module HSat.Problem.ProblemExpr (
  -- * ProblemExpr
  ProblemExpr(..),
  Convertable(..),
  IsProblem(..),
{-  BoolExpr,
  -- * Constructors
  mkBoolExprProblem,
  mkCNFBoolExpr,
  mkBSPBoolExpr,
  mkCNFProblem,
  -- * Query
  problemType,
  -- * Mutates
  changeProblemType,
  -- * Other functions
  problemToCNF,
  ProblemExpr2(..)-}
  ) where

--import qualified HSat.Problem.BSP as B
--import qualified HSat.Problem.BSP.CNF as C (CNF)
import           HSat.Problem.ProblemExpr.Internal
--import           HSat.Problem.ProblemType
{-
{-|
Returns the 'ProblemType' for the given type of 'ProblemExpr'
-}
problemType               :: ProblemExpr -> ProblemType
problemType = undefined
-}
