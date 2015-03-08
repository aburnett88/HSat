{-|
Module      : HSat.Problem.ProblemExpr
Description : The ProblemExpr type and its associated functions
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

The 'ProblemExpr' type contains all the 'Problem' types that can be represnted
in HSat. 
-}

module HSat.Problem.ProblemExpr (
  -- * ProblemExpr
  ProblemExpr(..),
  -- * Constructors
  mkCNFProblem,
  -- * Query
  problemType,
  -- * Mutators
  changeProblemType,
  -- * Other functions
  problemToCNF
  ) where

import           HSat.Printer
import qualified HSat.Problem.BSP.CNF as C (CNF)
import           HSat.Problem.ProblemType

{-|
A simple sum of types containing each of the problem types
-}
data ProblemExpr =
  CNFExpr C.CNF
  deriving (Eq,Show)

{-|
Returns the type of the Problem as described in 'ProblemType'
-}
problemType             :: ProblemExpr -> ProblemType
problemType (CNFExpr _) = CNF

{-|
A quick consructor for 'C.CNF' problems
-}
mkCNFProblem :: C.CNF -> ProblemExpr
mkCNFProblem = CNFExpr

{-|
Provides algorithms of changing one 'ProblemExpr' to another 'ProblemExpr'
-}
changeProblemType                     :: ProblemType ->
                                         ProblemExpr ->
                                         ProblemExpr
changeProblemType CNF cnf@(CNFExpr _) = cnf

{-|
Provides a quick method of getting a 'C.CNF' representation of a 'Problem'
-}
problemToCNF               :: ProblemExpr -> C.CNF
problemToCNF (CNFExpr cnf) = cnf

instance Printer ProblemExpr where
  compact (CNFExpr cnf)   = compact cnf
  noUnicode (CNFExpr cnf) = noUnicode cnf
  unicode (CNFExpr cnf)   = unicode cnf
