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
  BoolExpr(..),
  -- * Constructors
  mkBoolExprProblem,
  mkCNFBoolExpr,
  mkBSPBoolExpr,
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
import qualified HSat.Problem.BSP as B

{-|
A simple sum of types containing each of the problem types
-}
data ProblemExpr =
  BoolExprs BoolExpr
  deriving (Eq,Show)

data BoolExpr =
  BSPExpr B.BSP |
  CNFExpr C.CNF
  deriving (Eq,Show)

{-|
Returns the type of the Problem as described in 'ProblemType'
-}
problemType               :: ProblemExpr -> ProblemType
problemType (BoolExprs b) =
  case b of
    BSPExpr _ -> BSP
    CNFExpr _ -> CNF

{-|
A quick constructor to turn BoolExpr's into ProblemExprs
-}
mkBoolExprProblem :: BoolExpr -> ProblemExpr
mkBoolExprProblem = BoolExprs

{-|
A quick consructor for 'C.CNF' problems
-}
mkCNFBoolExpr :: C.CNF -> BoolExpr
mkCNFBoolExpr = CNFExpr

mkBSPBoolExpr :: B.BSP -> BoolExpr
mkBSPBoolExpr = BSPExpr

mkCNFProblem :: C.CNF -> ProblemExpr
mkCNFProblem = mkBoolExprProblem . mkCNFBoolExpr

{-|
Provides algorithms of changing one 'ProblemExpr' to another 'ProblemExpr'
-}
changeProblemType                     :: ProblemType ->
                                         ProblemExpr ->
                                         ProblemExpr
changeProblemType typ problem =
  case (problemType problem,typ) of
    (BSP,BSP) -> problem
    (BSP,CNF) -> error "Not written yet problemExpr l75"
    (CNF,BSP) -> error "Also not written yet l75 problemExpr"
    (CNF,CNF) -> problem

{-|
Provides a quick method of getting a 'C.CNF' representation of a 'Problem'
-}
problemToCNF               :: ProblemExpr -> C.CNF
problemToCNF (BoolExprs b) =
  case b of
    BSPExpr _ -> error "Not written yet l85"
    CNFExpr cnf -> cnf

instance Printer ProblemExpr where
  compact (BoolExprs b)   = compact b
  noUnicode (BoolExprs b) = noUnicode b
  unicode (BoolExprs b)   = unicode b

instance Printer BoolExpr where
  compact (BSPExpr bsp) = compact bsp
  compact (CNFExpr cnf) = compact cnf
  noUnicode (BSPExpr bsp) = noUnicode bsp
  noUnicode (CNFExpr cnf) = noUnicode cnf
  unicode (BSPExpr bsp) = unicode bsp
  unicode (CNFExpr cnf) = unicode cnf
