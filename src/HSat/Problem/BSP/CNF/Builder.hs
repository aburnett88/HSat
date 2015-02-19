{-|
Module      : HSat.Problem.BSP.CNF.Builder
Description : The Builder for the CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The CNF Builder type that builds up CNF instances
-}

module HSat.Problem.BSP.CNF.Builder (
  -- * Data Types
  CNFBuilderError,
  CNFBuildErr,
  CNFBuilder,
  -- * Safe Construction
  cnfBuilder,
  addLiteral,
  finishClause,
  finalise,
  -- * Mutable Constructors
  cnfBuilder',
  addLiteral',
  finishClause',
  finalise'
  ) where

import Data.Word
import HSat.Problem.BSP.CNF.Internal
import HSat.Problem.BSP.CNF.Builder.Internal
import HSat.Problem.BSP.Common

{-|
A type synonym for the Error type represented as an Either type denoting
failure or suces
-}
type CNFBuildErr = Either CNFBuilderError CNFBuilder

{-|
Creates an initial CNFBuilder with a set number of variables and clauses
-}
cnfBuilder     :: Integer -> Integer -> CNFBuildErr
cnfBuilder v c =
  if v < 0 || v > (toInteger (maxBound :: Word)) || c < 0 || c > (toInteger (maxBound :: Word)) then
    Left $ Initialisation v c else
    return $ cnfBuilder' v c
    
{-|
Creates an initial CNFBuilder with a set number of varialbes and clauses, but
returns the result purely
-}
cnfBuilder'     :: Integer -> Integer -> CNFBuilder
cnfBuilder' v c =
  CNFBuilder (fromInteger v) (fromInteger c) 0 emptyClauses emptyClause

{-|
Moves the current clause to the set of 'Clauses' and replaces this with an
empty 'Clause'
-}
finishClause     :: CNFBuilder -> CNFBuildErr
finishClause cnf =
  if clauseIsEmpty (getCurrClause cnf) && (
    getExptdClNumb cnf == getCurrClNumb cnf) then
    Left $
    IncorrectClauseNumber (getCurrClNumb cnf+1) (getExptdClNumb cnf) else
    return $ finishClause' cnf

{-|
Finishes the current 'Clause' and moves the pointer onto the next one
-}
finishClause'     :: CNFBuilder -> CNFBuilder
finishClause' cnf = g . f $ 
  cnf {
     getCurrClauses = newClauses,
     getCurrClause  = emptyClause
     }
  where
    f          = if clauseIsEmpty currClause then
                   incrClause else
                   id
    g cnfd     = if getCurrClNumb cnfd > getExptdClNumb cnfd then
                   cnfd {
                     getExptdClNumb = getCurrClNumb cnfd
                     } else
                   cnfd
    newClauses = clausesAddClause (getCurrClauses cnf) currClause
    currClause = getCurrClause cnf

{-|
Checks to see if the incorrect number of clauses has been delivered
-}
finalise     :: CNFBuilder -> Either CNFBuilderError CNF
finalise cnf =
  let currClNumb  = getCurrClNumb cnf
      exptdClNumb = getExptdClNumb cnf
  in if currClNumb == exptdClNumb then
       return . finalise' $ cnf else
       Left $ IncorrectClauseNumber currClNumb exptdClNumb

{-|
Turns the CNFBuilder into a CNF. If the current clause has elements in, this is
moved to the end of the 'Clauses'
-}
finalise'                           :: CNFBuilder -> CNF
finalise' (CNFBuilder v _ curr cl c) =
  if clauseIsEmpty c then
    CNF v curr cl else
    CNF v curr $ clausesAddClause cl c

{-|
Incrmeents the current clause count by one
-}
incrClause     :: CNFBuilder -> CNFBuilder
incrClause cnf = cnf {
  getCurrClNumb = (1+) (getCurrClNumb cnf)
  }

{-|
Checks the literal to make sure that it is within range and, if ti is, create
the new CNFBuilder.

Else, throw an error
-}
addLiteral       :: Integer -> CNFBuilder -> CNFBuildErr
addLiteral lit cnf =
  let lit' = abs lit
      maxVar = getExptdMaxVar cnf
  in if lit' == 0 || lit' > (toInteger maxVar) then
       Left $ VarOutsideRange lit' maxVar else
       return . addLiteral' lit $ cnf

{-|
adds the literal to the clause. If the ltieral is outside the range
denoted by the CNFBuilder, the range is increased to incorporate it
-}
addLiteral'       :: Integer -> CNFBuilder -> CNFBuilder
addLiteral' lit cnf =
  (\cnf' ->
    let l = mkLiteralFromInteger lit
        exptdMaxVar = getExptdMaxVar cnf'
        l' = getWord . getVariable $ l
    in cnf' {
      getCurrClause  = clauseAddLiteral (getCurrClause cnf') l,
      getExptdMaxVar = if exptdMaxVar < l' then
                         l' else
                         exptdMaxVar
      }
  )
  $ (if clauseIsEmpty . getCurrClause $ cnf then
      incrClause else
      id
      )
  cnf
    
