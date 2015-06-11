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
  IsProblem(..),
  -- * Conversions
  fromCNFtoBSP,
  fromBSPtoCNF,
  getWriterInfo
  ) where

import qualified HSat.Problem.BSP.CNF as C (mkCNFFromClauses)
import HSat.Problem.BSP.CNF (CNF)
import HSat.Problem.BSP.Internal
import qualified Data.Vector as V
import HSat.Problem.BSP.Common
import Control.Monad.Catch (MonadThrow)
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)

data ProblemExpr = forall p. IsProblem p => ProblemExpr p

getWriterInfo :: ProblemExpr -> Maybe (FilePath,Text)
getWriterInfo (ProblemExpr a) = getWriterInfo a

class IsProblem problem where
  fromCNF :: CNF -> problem
  toCNF :: problem -> CNF
  supportedFile :: (MonadThrow m) => problem -> Maybe (FilePath,Text,Parser (m problem))--, Parser (m problem))
  supportedFile _ = Nothing

instance IsProblem BSP where
  fromCNF =  fromCNFtoBSP
  toCNF = fromBSPtoCNF

getWriterInfo' :: (IsProblem p) => p -> Maybe (FilePath,Text)
getWriterInfo' p =
  getInfo $ supportedFile p

getInfo :: (IsProblem p) => (Maybe (FilePath,Text,Parser (Maybe p))) -> Maybe (FilePath,Text)
getInfo = undefined
  
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
