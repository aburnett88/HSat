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
import HSat.Printer
import HSat.Problem.BSP.Common
import HSat.Problem.BSP.Common.Clauses.Internal (printClausesWithContext)
import HSat.Validate

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

instance Validate CNF where
  validate (CNF maxVar clauseNumb clauses) =
    let actualClauseNumb = getSizeClauses clauses
        actualMaxVar     = findMaxVar clauses
    in (actualClauseNumb == clauseNumb) &&
       (actualMaxVar     <= maxVar)     &&
       (validate clauses)

noUnicodeAnd,unicodeAnd,noUnicodeOr,unicodeOr :: String
noUnicodeAnd                                  = "/\\"
unicodeAnd                                    = "∧"
noUnicodeOr                                   = "\\/"
unicodeOr                                     = "∨"

compactCNF,noUnicodeCNF,unicodeCNF :: String
compactCNF                         = "CNF"
noUnicodeCNF                       = ""
unicodeCNF                         = "CNF"

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
    printClausesWithContext noUnicodeAnd noUnicodeOr maxVar noUnicode clauses
  unicode (CNF maxVar clauseNumb clauses)   =
    text unicodeCNF          <> line  <>
    text unicodeVariable     <> colon <+>
    (text $ show maxVar)     <> line  <>
    text unicodeClNumb       <> colon <+>
    (text $ show clauseNumb) <> line  <>
    printClausesWithContext unicodeAnd unicodeOr maxVar unicode clauses
