{-# LANGUAGE
    OverloadedStrings,
    RecordWildCards
    #-}

{-|
Module      : HSat.Problem.Instances.CNF.Internal
Description : The CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the 'CNF' internal representation
-}

module HSat.Problem.Instances.CNF.Internal (
  CNF(..)
  ) where

import HSat.Printer
import HSat.Problem.Instances.Common
import HSat.Problem.Instances.Common.Clauses.Internal (printClausesWithContext)

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
  compact   = docCNF Compact
  noUnicode = docCNF NoUnicode
  unicode   = docCNF Unicode

docCNF               :: PrinterType -> CNF -> Doc
docCNF pType CNF{..} =
  title    <+>
  variable <+> toDoc getMaxVar     <> space' <>
  clause   <+> toDoc getClauseNumb <> line   <>
  clauses
  where
    title   :: Doc
    title = "CNF"
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
      Compact -> compact getClauses
      NoUnicode ->
        printClausesWithContext noUnicodeAnd noUnicodeOr getMaxVar noUnicode getClauses
      Unicode -> printClausesWithContext unicodeAnd unicodeOr getMaxVar unicode getClauses
