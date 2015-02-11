{-|
Module      : HSat.Problem.BSP.CNF.Internal
Description : The CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the 'CNF' internal representation
-}

module HSat.Problem.BSP.CNF.Internal (
  CNF(..)
  ) where

import Data.Word
import HSat.Problem.BSP.Common
import HSat.Printer
import qualified Data.Vector as V

{-|
The Conjunctive Normal Form type. It is much like a 'Clauses'
type but with additional information.
-}
data CNF = CNF {
  -- | The range of 'Variable's within the CNF Problem
  getNoVars :: Word,
  -- | The number of 'Clause's in the problem
  getNoClauses :: Word,
  -- | The problems 'Clauses' themselves
  getClauses :: Clauses
  } deriving (Eq,Show)

instance Printer CNF where
  compact (CNF v c cl) =
    text "CNF V:" <+>
    (text . show $ v) <+>
    text "C:" <+>
    (text . show $ c) <>
    line <>
    compact cl
  noUnicode (CNF v c cl) =
    text "Variables:" <+>
    (text . show $ v) <+>
    text "Clauses:" <+>
    (text . show $ c) <>
    line <>
    writeClauses (text " /\\ ") (text " \\/ ") v noUnicode cl
  unicode (CNF v c cl) =
    text "Variable:" <+>
    (green . text . show $ v) <+>
    text "Clauses:" <+>
    (text . show $ c) <>
    line <>
    writeClauses (text " /\\ ") (text " \\/ ") v unicode cl

writeClauses :: Doc -> Doc -> Word -> (Literal -> Doc) -> Clauses -> Doc
writeClauses sepCl sepC vars f cl =
  encloseSep empty empty sepCl clausesDoc
  where
    --seprate by the separator
    clausesDoc = map (encloseSep lparen rparen sepC) . function $ literalList
    --get a list of list of literals
    literalList = map (V.toList . getVectLiteral) .  V.toList . getVectClause $ cl :: [[Literal]]
    --The size that the greatest var will take up in the printed output
    vSize = (1+) . length . show $ vars
    --And finally convert a lit of list of literals to a list of list of docs
    function =
      map (map (
              \lit -> (text . replicate (
                          vSize - (
                             length . show . literalToInteger $ lit)) $ ' ')
                      <>
                      f lit))

