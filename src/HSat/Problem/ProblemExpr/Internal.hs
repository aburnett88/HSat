{-# LANGUAGE 
  MultiParamTypeClasses, 
  FunctionalDependencies, ScopedTypeVariables, PartialTypeSignatures,
  ExistentialQuantification #-}

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
  -- * Conversions
  fromCNFtoBSP,
  fromBSPtoCNF,
  IsProblem(..),
  fromProblem,
  Convertable(..),
  ) where

import qualified HSat.Problem.BSP.CNF as C (mkCNFFromClauses)
import HSat.Problem.BSP.CNF (CNF)
import HSat.Problem.BSP.Internal
import qualified Data.Vector as V
import HSat.Problem.BSP.Common
import Control.Monad.Catch 
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Data.Typeable
import Control.Monad.Random


data ProblemExpr = forall s. (IsProblem s, Typeable s) => ProblemExpr {isProblem :: s}

fromProblem :: (Typeable s, IsProblem s) => ProblemExpr -> Maybe s
fromProblem (ProblemExpr p) = cast p

class Config c where
  ccc :: c -> Int

class (Config config) => IsProblem problem config  | problem -> config where
  fromCNF :: CNF -> problem
  toCNF :: problem -> CNF
  getWriter :: problem -> Maybe (FilePath, Text)
  getWriter _ = Nothing
  getParser :: (MonadThrow m) => Maybe (Parser (m problem))
  getParser = Nothing
  make :: (MonadThrow m, MonadRandom m) => config -> m problem

class (IsProblem a b, IsProblem c d) => Convertable a b c d where
  convert :: a -> b
  convert = undefined
  
  

instance IsProblem BSP where
  fromCNF =  fromCNFtoBSP
  toCNF = fromBSPtoCNF

instance IsProblem CNF where
  fromCNF = id
  toCNF = id
  getWriter _ = undefined
  getParser = undefined

{-|
Takes a 'B.BSP' and converts it to a 'C.CNF'
-}
fromBSPtoCNF :: BSP -> CNF
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

intoCNF :: [[BSP]] -> CNF
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
fromCNFtoBSP :: CNF -> BSP
fromCNFtoBSP = error "not written yet fromCNFtoBSP"
