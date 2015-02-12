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
  CNF(..),
  validate
  ) where

import           Data.Word
import qualified Data.Vector as V
import           HSat.Printer
import           HSat.Problem.BSP.Common

{-|
The Conjunctive Normal Form type. It is much like a 'Clauses'
type but with additional information
-}
data CNF = CNF {
  -- | The maximum 'Word' allowed in a 'Variable' in 'getClauses'
  getMaxVar     :: Word   ,
  -- | The number of 'Clause's in the problem
  getClauseNumb :: Word   ,
  -- | The problems 'Clauses' themselves
  getClauses    :: Clauses
  } deriving (Eq,Show)

noUnicodeAnd,unicodeAnd,noUnicodeOr,unicodeOr :: String
noUnicodeAnd                                  = "/\\"
unicodeAnd                                    = "/\\"
noUnicodeOr                                   = "\\/"
unicodeOr                                     = "\\/"

compactCNF,noUnicodeCNF,unicodeCNF :: String
compactCNF                         = "CNF"
noUnicodeCNF                       = ""
unicodeCNF                         = ""

compactVariable,noUnicodeVariable,unicodeVariable :: String
compactVariable                                   = "V"
noUnicodeVariable                                 = "Variables"
unicodeVariable                                   = "Variables"

compactClNumb,noUnicodeClNumb,unicodeClNumb :: String
compactClNumb                               = "C"
noUnicodeClNumb                             = "Clauses"
unicodeClNumb                               = "Clauses"

instance Printer CNF where
  compact (CNF maxVar clauseNumb clauses)   =
    text compactCNF                  <+>
    text compactVariable <> colon    <+>
    (text $ show maxVar)             <+>
    text compactClNumb <> colon      <+>
    (text $ show clauseNumb) <> line <>
    compact clauses
  noUnicode (CNF maxVar clauseNumb clauses) =
    text noUnicodeCNF                 <+>
    text noUnicodeVariable   <> colon <+>
    (text $ show maxVar)     <> line  <>
    text noUnicodeClNumb     <> colon <+>
    (text $ show clauseNumb) <> line  <>
    writeClauses noUnicodeAnd noUnicodeOr maxVar noUnicode clauses
  unicode (CNF maxVar clauseNumb clauses)   =
    text unicodeCNF                   <+>
    text unicodeVariable     <> colon <+>
    (text $ show maxVar)     <> line  <>
    text unicodeClNumb       <> colon <+>
    (text $ show clauseNumb) <> line  <>
    writeClauses unicodeAnd unicodeOr maxVar unicode clauses

writeClauses :: String -> String -> Word -> (Literal -> Doc) -> Clauses -> Doc
writeClauses sepClauses sepClause maxVar function clauses =
  encloseSep empty empty (text sepClauses) clausesDoc
  where
    --Padding takes a Literal and deicdes how much spacing it needs to line up
    padding :: Literal -> Doc
    padding lit =
      let maxVarLen  = length $ show maxVar
          varLen     = length . show . getWord $ getVariable lit
          difference = maxVarLen - varLen
      in (text $ replicate difference ' ') <> (function lit)
    --the [Doc] of [Clause]
    clausesDoc :: [Doc]
    clausesDoc = map (encloseSep lparen rparen (text sepClause)) literalDocs
    literalDocs :: [[Doc]]
    literalDocs = map (map padding) literalLists
    literalLists :: [[Literal]]
    literalLists = V.toList . V.map (V.toList . getVectLiteral) .
                   getVectClause $ clauses

{-|
Returns 'True' if the number of 'Clauses' is consistant with the number stated
in the 'CNF' and the maximum 'Variab'e within the 'Clauses' is less than or
equlal to getVarBound
-}
validate :: CNF -> Bool
validate cnf =
  let v = findMaxVar $ getClauses cnf
      c = getSizeClauses $ getClauses cnf
  in getMaxVar cnf >= v &&
     getClauseNumb cnf == c
