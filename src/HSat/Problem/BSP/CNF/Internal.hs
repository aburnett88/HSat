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
  } deriving (Eq)

instance Show CNF where
  showsPrec = show'

noUnicodeAnd,unicodeAnd,noUnicodeOr,unicodeOr :: String
noUnicodeAnd                                  = "/\\"
unicodeAnd                                    = "∧"
noUnicodeOr                                   = "\\/"
unicodeOr                                     = "∨"

instance Printer CNF where
  compact = docCNF Compact
  noUnicode = docCNF NoUnicode
  unicode = docCNF Unicode

docCNF :: PrinterType -> CNF -> Doc
docCNF pType (CNF maxVar clNumb cl) =
  title <+>
  variable <+> word maxVar <> space' <>
  clause <+> word clNumb <> line <>
  clauses
  where
    title :: Doc
    title = text "CNF"
    variable :: Doc
    variable = text $ case pType of
      Compact -> "V"
      _ -> "Variables"
    space' :: Doc
    space' = case pType of
      Compact -> space
      _ -> line
    clause :: Doc
    clause = text $ case pType of
      Compact -> "C"
      _ -> "Clauses"
    clauses :: Doc
    clauses = case pType of
      Compact -> compact cl
      NoUnicode ->
        printClausesWithContext noUnicodeAnd noUnicodeOr maxVar noUnicode cl
      Unicode -> printClausesWithContext unicodeAnd unicodeOr maxVar unicode cl
