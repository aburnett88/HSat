{-# LANGUAGE RecordWildCards #-}

{-|
Module      : HSat.Problem.BSP.CNF.Builder.Internal
Description : The Internal CNFBuilder module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the definition of the CNFBuilder and CNFBuilderError
data types, as well as helper functions that define what actions can be
performed on a CNFBuilder
-}

module HSat.Problem.Instances.CNF.Builder.Internal (
  -- * Definitions
  CNFBuilder(..),
  CNFBuilderError(..),
  -- * Query Functions
  canAddLiteral,       -- :: CNFBuilder -> Bool
  canFinishClause,     -- :: CNFBuilder -> Bool
  canFinalise          -- :: CNFBuilder -> Bool
  ) where

import           HSat.Printer
import           HSat.Problem.Instances.Common
import           HSat.Problem.Instances.Common.Clause.Internal (
  printClauseWithContext)
import           HSat.Problem.Instances.Common.Clauses.Internal (
  printClausesWithContext)
import Control.Monad.Catch

{-|
A 'CNFBuilder' represents a 'CNF' as it is being constructed. This data type
is for when the number of expected 'Clauses' and maximum 'Variable' within
a 'Problem' have been defined beforehand, and this data structure is used to
enforce these constraints on the problem
-}
data CNFBuilder = CNFBuilder {
  -- | The maximum 'Word' within a 'Variable' expected
  getExptdMaxVar :: Word   ,
  -- | The expected number of 'Clauses'
  getExptdClNumb :: Word   ,
  -- | The current number of 'Clauses'
  getCurrClNumb  :: Word   ,
  -- | The current 'Clauses'
  getCurrClauses :: Clauses,
  -- | The current 'Clause' 'Literal's are added to
  getCurrClause  :: Clause
  }
  deriving (Eq)

instance Show CNFBuilder where
  showsPrec = show'

{-|
Denotes whether a 'Literal' can be added to the 'CNFBuilder' and it
remains a valid 'CNFBuilder'
-}
canAddLiteral                       :: CNFBuilder -> Bool
canAddLiteral CNFBuilder{..}
  | getCurrClNumb < getExptdClNumb  = True
  | getCurrClNumb == getExptdClNumb = not $ clauseIsEmpty getCurrClause
  | otherwise                       = False

{-|
Returns 'True' if the CNFBuilder can be finalised (turned into a 'CNF')
-}
canFinalise                :: CNFBuilder -> Bool
canFinalise CNFBuilder{..} =
  getCurrClNumb == getExptdClNumb
  
{-|
Returns 'True' if a 'Clause' can be finished. Empty clauses can also be added
to the 'CNFBuilder'
-}
canFinishClause :: CNFBuilder -> Bool
canFinishClause = canAddLiteral

instance Printer CNFBuilder where
  compact builder   = printCNFBuilder builder   Compact
  noUnicode builder = printCNFBuilder builder NoUnicode
  unicode builder   = printCNFBuilder builder   Unicode

{-|
Prints the CNFBuilder with a given 'PrinterType'
-}
printCNFBuilder :: CNFBuilder -> PrinterType -> Doc
printCNFBuilder (CNFBuilder{..}) pType =
  title          <> line <>
  maxVar         <> line <>
  clauses        <> line <>
  currentClauses <> line <>
  currentClause  <> line <>
  facts 
  where
    title :: Doc
    title = text "CNFBuilder"
    maxVar :: Doc
    maxVar =
      case pType of
        Compact -> text "Max Var"
        _       -> text "Maximum Variable"
      <> colon <+> toDoc getExptdMaxVar
    clauses :: Doc
    clauses =
      case pType of
        Compact -> text "Clauses" <> colon               <+>
                   toDoc getCurrClNumb                    <>
                   text "/"                              <>
                   toDoc getExptdClNumb
        _       -> text "Clauses"   <> colon             <+>
                   toDoc getExptdClNumb <> line           <>
                   text "Current Clause Counnt" <> colon <+>
                   toDoc getCurrClNumb
    currentClauses :: Doc
    currentClauses = case pType of
      Compact -> compact getCurrClauses
      _ -> printClausesWithContext "and" "or"
           getExptdMaxVar func getCurrClauses
    currentClause :: Doc
    currentClause = case pType of
      Compact -> compact getCurrClause
      _ -> printClauseWithContext "OR" getExptdMaxVar func getCurrClause
    func :: Literal -> Doc
    func = case pType of
      Compact -> compact
      NoUnicode -> noUnicode
      Unicode -> unicode
    facts :: Doc
    facts = empty
    
{-|
CNFBuilderError describes the errors that can be thrown by a CNFBuidler.
-}
data CNFBuilderError =
  -- | When the incorrect number of 'Clauses' has been defined.
  IncorrectClauseNumber Word Word |
  -- | When a 'Literal' is constructed outside the range specified
  VarOutsideRange Integer Word    |
  -- | When the number of 'Variable's or
  -- | 'Clause's are not within the bounds of a word
  Initialisation Integer Integer
  deriving (Eq)

instance Exception CNFBuilderError

instance Show CNFBuilderError where
  showsPrec = show'

instance Printer CNFBuilderError where
  compact err   = printBuildErr err   Compact
  noUnicode err = printBuildErr err NoUnicode
  unicode err   = printBuildErr err   Unicode

{-|
Prints the given 'CNFBuilderError' with a gien 'PrinterType'
-}
printBuildErr :: CNFBuilderError -> PrinterType -> Doc
printBuildErr builderErr pType =
  case builderErr of
    IncorrectClauseNumber gotten expected ->
      errorDoc pType $
        text "Incorrect Number of Clauses"             <+>
        text "Expected"     <> colon <+> toDoc expected <+>
        text "Actual Value" <> colon                   <+>
        toDoc gotten
    VarOutsideRange gotten expected ->
      let le = text "<"
          leq = text $
                case pType of
                  Unicode -> "≤"
                  _ -> "<="
      in errorDoc pType $
           text "Variable outside range"        <>  colon <+>
           toDoc (0::Word) <+> le <+> text (show gotten) <+> leq   <+>
           toDoc expected
    Initialisation variables clauses ->
      errorDoc pType $
        text "Initialisation of arguments incorrect" <+>
        text "Argumnets:" <+> text (show variables)  <+> text (show clauses)

