{-|
Module      : HSat.Problem.ProblemExpr.Internal
Description : Internal definition of the ProblemExpr type
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Contains the definition of the 'ProblemExpr' type
-}

module HSat.Problem.ProblemExpr.Internal (
  -- * Data Type
  ProblemExpr(..),
  BoolExpr(..),
  -- * Conversions
  fromCNFtoBSP,
  fromBSPtoCNF
  ) where

import           HSat.Printer
import qualified HSat.Problem.BSP as B
import qualified HSat.Problem.BSP.CNF as C (CNF)

{-|
A sum type containing all 'ProblemExpr' sub-types
-}
data ProblemExpr =
  BoolExprs BoolExpr
  deriving (Eq,Show)

{-|
General sum type for 'BoolExpr's such as general Boolean Formula
and Conjunctive Normal Form formula
-}
data BoolExpr =
  -- | General Boolean Formula
  BSPExpr B.BSP |
  -- | Specific Conjunctive Normal Form formula
  CNFExpr C.CNF
  deriving (Eq,Show)

instance Printer ProblemExpr where
  compact (BoolExprs b)   = compact b
  noUnicode (BoolExprs b) = noUnicode b
  unicode (BoolExprs b)   = unicode b

instance Printer BoolExpr where
  compact (BSPExpr bsp)   = compact bsp
  compact (CNFExpr cnf)   = compact cnf
  noUnicode (BSPExpr bsp) = noUnicode bsp
  noUnicode (CNFExpr cnf) = noUnicode cnf
  unicode (BSPExpr bsp)   = unicode bsp
  unicode (CNFExpr cnf)   = unicode cnf

fromBSPtoCNF :: B.BSP -> C.CNF
fromBSPtoCNF = error "not written yet fromBSPtoCNF"

fromCNFtoBSP :: C.CNF -> B.BSP
fromCNFtoBSP = error "not written yet fromCNFtoBSP"
