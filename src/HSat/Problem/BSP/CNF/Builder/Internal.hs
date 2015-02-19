{-|
Module      : HSat.Problem.BSP.CNF.Builder.Internal
Description : The Internal CNFBuilder module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the definition for the CNFBuidler and CNFBuilderError
types
-}

module HSat.Problem.BSP.CNF.Builder.Internal (
  CNFBuilder(..),
  CNFBuilderError(..),
  canAddLiteral,
  canFinishClause,
  canFinalise
  ) where

import qualified Data.Vector as V
import           Data.Word
import           HSat.Printer
import           HSat.Problem.BSP.Common
import           HSat.Problem.BSP.Common.Clause.Internal (
  printClauseWithContext)
import           HSat.Problem.BSP.Common.Clauses.Internal (
  printClausesWithContext)
import           HSat.Validate

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
  -- | The current number of 'Clauses'genCNFBuilderFinalise 10 10 10 10)
  getCurrClNumb  :: Word   ,
  -- | The current 'Clauses'
  getCurrClauses :: Clauses,
  -- | The current 'Clause' 'Literal's are added to
  getCurrClause :: Clause
  }
  deriving (Eq)

instance Show CNFBuilder where
  showsPrec = show'

{-|
Denotes whether a 'Literal' can be added to the 'CNFBuilder' and it
remains a valid 'CNFBuilder'
-}
canAddLiteral :: CNFBuilder -> Bool
canAddLiteral builder =
  let curr       = getCurrClNumb builder
      exptd      = getExptdClNumb builder
      currClause = getCurrClause builder
  in (curr <  exptd) || (
     (curr == exptd) &&
     not (clauseIsEmpty currClause)
     )

{-|
Returns 'True' if the CNFBuilder can be finalised (turned into a 'CNF')
-}
canFinalise         :: CNFBuilder -> Bool
canFinalise builder =
  let curr  = getCurrClNumb builder
      exptd = getExptdClNumb builder
  in (curr  == exptd)

{-|
Returns 'True' if a 'Clause' can be finished. Empty clauses can also be added
to the 'CNFBuilder'
-}
canFinishClause :: CNFBuilder -> Bool
canFinishClause = canAddLiteral

instance Validate CNFBuilder where
  validate (CNFBuilder
            exptdMaxVar
            exptdClNumb
            currClNumb
            currClauses
            currClause) =
    let computedSize = (getSizeClauses currClauses + (
                           if clauseIsEmpty currClause then
                             0 else
                             1)
                        )
    in (exptdClNumb >= currClNumb)                        &&
       V.all testVarInRange (getVectClause currClauses) &&
       (computedSize == currClNumb)                       &&
       testVarInRange currClause                        &&
       validate currClauses                             &&
       validate currClause
    where
      testVarInRange :: Clause -> Bool
      testVarInRange cl = V.all (varInRange exptdMaxVar) .
                          V.map getVariable $ getVectLiteral cl

instance Printer CNFBuilder where
  compact builder   = printCNFBuilder builder   Compact
  noUnicode builder = printCNFBuilder builder NoUnicode
  unicode builder   = printCNFBuilder builder   Unicode

{-|
Prints the CNFBuilder with a given 'PrinterType'
-}
printCNFBuilder :: CNFBuilder -> PrinterType -> Doc
printCNFBuilder (CNFBuilder
                 exptdMaxVar
                 exptdClNumb
                 currClNumb
                 currClauses
                 currClause) pType =
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
      <> colon <+> word exptdMaxVar
    clauses :: Doc
    clauses =
      case pType of
        Compact -> text "Clauses" <> colon               <+>
                   word currClNumb                        <>
                   text "/"                               <>
                   word exptdClNumb
        _       -> text "Clauses"   <> colon             <+>
                   word exptdClNumb <> line               <>
                   text "Current Clause Counnt" <> colon <+>
                   word currClNumb
    currentClauses :: Doc
    currentClauses = case pType of
      Compact -> compact currClauses
      _ -> printClausesWithContext "and" "or" exptdMaxVar func currClauses
    currentClause :: Doc
    currentClause = case pType of
      Compact -> compact currClause
      _ -> printClauseWithContext "OR" exptdMaxVar func currClause
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
  LitOutsideRange Word Word
  deriving (Eq)

instance Show CNFBuilderError where
  showsPrec = show'

instance Validate CNFBuilderError where
  validate (IncorrectClauseNumber gotten expected) =
    expected /= gotten
  validate (LitOutsideRange gotten expected)       =
    (expected < gotten) ||
    (gotten == 0)

instance Printer CNFBuilderError where
  compact err   = printBuildErr err   Compact
  noUnicode err = printBuildErr err NoUnicode
  unicode err   = printBuildErr err   Unicode

{-|
Prints the given 'CNFBuilderError' with a gien 'PrinterType'
-}
printBuildErr :: CNFBuilderError -> PrinterType -> Doc
printBuildErr (IncorrectClauseNumber
               gotten
               expected) pType =
  errorDoc pType $
    text "Incorrect Number of Clauses"               <+>
    text "Expected"     <> colon <+> word expected <+>
    text "Actual Value" <> colon                     <+>
    word gotten
printBuildErr (LitOutsideRange gotten expected) pType =
  errorDoc pType $
    text "Variable outside range"    <> colon  <+>
    word 0 <+> le <+> word gotten <+>   leq  <+>
    word expected
  where
    le :: Doc
    le = text "<"
    leq :: Doc
    leq = text $
      case pType of
        Unicode -> "≤"
        _       -> "<="


