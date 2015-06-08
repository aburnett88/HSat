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
import qualified HSat.Problem.BSP.CNF as C (CNF,mkCNFFromClauses)
import HSat.Problem.BSP.Internal
import qualified Data.Vector as V
import HSat.Problem.BSP.Common

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
  compact (CNFExpr c)   = compact c
  noUnicode (BSPExpr bsp) = noUnicode bsp
  noUnicode (CNFExpr f) = noUnicode f
  unicode (BSPExpr bsp)   = unicode bsp
  unicode (CNFExpr c)   = unicode c

{-|
Takes a 'B.BSP' and converts it to a 'C.CNF'
-}
fromBSPtoCNF :: B.BSP -> C.CNF
fromBSPtoCNF bsp =
  let bsp' = map listY . listX . simplify . cnf . nnf . removeIf . removeIff $ bsp
  in intoCNF bsp'

listX :: BSP -> [BSP]
listX (And l@(And _ _) r@(Or _ _)) = listX l ++ [r]
listX (And l@(Or _ _) r@(And _ _)) = [l] ++ listX r
listX (And l@(Or _ _) r@(Or _ _)) = [l] ++ [r]
listX b = [b]

listY :: BSP -> [BSP]
listY (Or l@(Or _ _) r) = listY l ++ [r]
listY (Or l r@(Or _ _)) = [l] ++ listY r
listY (Or l r) = [l] ++ [r]
listY a = [a]

intoCNF :: [[BSP]] -> C.CNF
intoCNF [[(Bool' True)]] = C.mkCNFFromClauses $ emptyClauses
intoCNF [[(Bool' False)]] = C.mkCNFFromClauses $ emptyClauses
intoCNF bsp = C.mkCNFFromClauses . mkClauses . V.fromList  $ map toClauseList bsp
  where
    toClauseList :: [BSP] -> Clause
    toClauseList [(Bool' False)] = emptyClause
    toClauseList [(Bool' True)] = emptyClause
    toClauseList bsps = mkClauseFromLits $ map intoCNF' bsps
    intoCNF' :: BSP -> Literal
    intoCNF' (Variable' v) = mkLiteral pos v
    intoCNF' (Not (Variable' v)) = mkLiteral neg v
    intoCNF' a = error (show a)

{-|
Takes a 'C.CNF' and converts it to a 'B.BSP'
-}
fromCNFtoBSP :: C.CNF -> B.BSP
fromCNFtoBSP = error "not written yet fromCNFtoBSP"
