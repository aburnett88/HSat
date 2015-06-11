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
  ProblemConvert(..),
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

class (IsProblem a, IsProblem b) => ProblemConvert a b where
  conversion :: a -> b
  conversion = fromCNF . toCNF
{-
{-|
Returns the 'ProblemType' for the given type of 'ProblemExpr'
-}
problemType               :: ProblemExpr -> ProblemType
problemType (BoolExprs b) =
  case b of
    BSPExpr _ -> BSP
    CNFExpr _ -> CNF

{-|
Creates 'ProblemExpr' from a 'BoolExpr'
-}
mkBoolExprProblem :: BoolExpr -> ProblemExpr
mkBoolExprProblem = BoolExprs

{-|
Constructs a 'BoolExpr' from a 'C.CNF' data type
-}
mkCNFBoolExpr :: C.CNF -> BoolExpr
mkCNFBoolExpr = CNFExpr

{-|
Constructs a 'BoolExpr' from a 'B.BSP' data type
-}
mkBSPBoolExpr :: B.BSP -> BoolExpr
mkBSPBoolExpr = BSPExpr

{-|
Constructs a 'ProblemExpr' from a 'C.CNF' data type
-}
mkCNFProblem :: C.CNF -> ProblemExpr
mkCNFProblem = mkBoolExprProblem . mkCNFBoolExpr

{-|
Converts the internal structure of a 'ProblemExpr' to the requested one
-}
changeProblemType                     :: ProblemType ->
                                         ProblemExpr ->
                                         ProblemExpr
changeProblemType typ problem =
  case (problem,typ) of
    (BoolExprs (BSPExpr _),BSP) -> problem
    (BoolExprs (CNFExpr _),CNF) -> problem
    (BoolExprs (BSPExpr b),CNF) -> mkCNFProblem $ fromBSPtoCNF b
    (BoolExprs (CNFExpr c),BSP) -> mkBoolExprProblem . mkBSPBoolExpr  $ fromCNFtoBSP c

{-|
Converts a 'ProblemExpr' into a generic 'C.CNF' data type
-}
problemToCNF               :: ProblemExpr -> C.CNF
problemToCNF (BoolExprs bool) =
  case bool of
    BSPExpr b   -> fromBSPtoCNF b
    CNFExpr cnf -> cnf


-}
