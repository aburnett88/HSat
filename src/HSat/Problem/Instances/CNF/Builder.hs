{-# LANGUAGE
    RecordWildCards,
    MultiWayIf
    #-}

{-|
Module      : HSat.Problem.Instances.CNF.Builder
Description : The Builder for the CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

The CNF Builder type that builds up CNF instances
-}

module HSat.Problem.Instances.CNF.Builder (
  -- * Data Types
  CNFBuilder,
  -- * Safe Construction
  cnfBuilder   , -- :: (MonadThrow m) => Integer -> Integer -> m CNFBuilder
  addLiteral   , -- :: (MonadThrow m) => Integer -> CNFBuilder -> m CNFBuilder
  finishClause , -- :: (MonadThrow m) => CNFBuilder -> m CNFBuilder
  finalise,      -- :: (MonadThrow m) => CNFBuilder -> m CNF
  -- * Mutable Constructors
  cnfBuilder'  , -- :: Integer -> Integer -> CNFBuilder
  addLiteral'  , -- :: Integer -> CNFBuilder -> CNFBuilder
  finishClause', -- :: CNFBuilder -> CNFBuilder
  finalise'      -- :: CNFBuilder -> CNF
  ) where

import Control.Monad.Catch
import HSat.Problem.Instances.CNF.Builder.Internal
import HSat.Problem.Instances.CNF.Internal
import HSat.Problem.Instances.Common

{-|
Creates an initial CNFBuilder with a set number of variables and clauses
-}
cnfBuilder     :: (MonadThrow m) => Integer -> Integer -> m CNFBuilder
cnfBuilder v c = 
  if v < 0 || v > toInteger (maxBound :: Word) ||
     c < 0 || c > toInteger (maxBound :: Word) then
    throwM $ Initialisation v c else
    return $ cnfBuilder' v c

{-|
Creates an initial CNFBuilder with a set number of variables and clauses, but
returns the result purely
-}
cnfBuilder'     :: Integer -> Integer -> CNFBuilder
cnfBuilder' v c =
  CNFBuilder (fromInteger v) (fromInteger c) 0 emptyClauses emptyClause

{-|
Moves the current clause to the set of 'Clauses' and replaces this with an
empty 'Clause'
-}
finishClause                    :: (MonadThrow m) => CNFBuilder -> m CNFBuilder
finishClause cnf@CNFBuilder{..} =
  if clauseIsEmpty getCurrClause && (getExptdClNumb == getCurrClNumb) then
    throwM $ IncorrectClauseNumber (getCurrClNumb + 1) getExptdClNumb else
    return $ finishClause' cnf

{-|
Finishes the current 'Clause' and moves the pointer onto the next one
-}
finishClause'                    :: CNFBuilder -> CNFBuilder
finishClause' cnf@CNFBuilder{..} = incrExptd .
  (\cnf2 -> cnf2 {
    getCurrClauses = newClauses,
    getCurrClause  = emptyClause
    }) $ incrClause' cnf
  where
    newClauses     :: Clauses
    newClauses     = clausesAddClause getCurrClauses getCurrClause

incrExptd                    :: CNFBuilder -> CNFBuilder
incrExptd cnf@CNFBuilder{..} = if getCurrClNumb > getExptdClNumb then
                                 cnf {
                                   getExptdClNumb = getCurrClNumb
                                   } else
                                 cnf

--Internal function that increments a CNFBuilder if the clause is empty
incrClause'                    :: CNFBuilder -> CNFBuilder
incrClause' cnf@CNFBuilder{..} = if clauseIsEmpty getCurrClause then
                               incrClause cnf else
                               cnf

{-|
Checks to see if the incorrect number of clauses has been delivered
-}
finalise                    :: (MonadThrow m) => CNFBuilder -> m CNF
finalise cnf@CNFBuilder{..} =
  if getCurrClNumb == getExptdClNumb then
    return . finalise' $ cnf else
    throwM $ IncorrectClauseNumber getCurrClNumb getExptdClNumb

{-|
Turns the CNFBuilder into a CNF. If the current clause has elements in, this is
moved to the end of the 'Clauses'
-}
finalise'                            :: CNFBuilder -> CNF
finalise' (CNFBuilder v _ curr cl c) =
  if clauseIsEmpty c then
    CNF v curr cl else
    CNF v curr $ clausesAddClause cl c

{-|
Increments the current clause count by one
-}
incrClause                    :: CNFBuilder -> CNFBuilder
incrClause cnf@CNFBuilder{..} = cnf {
  getCurrClNumb = getCurrClNumb + 1
  }

{-|
Checks the literal to make sure that it is within range and, if ti is, create
the new CNFBuilder.

Else, throw an error
-}
addLiteral       :: (MonadThrow m) => Integer -> CNFBuilder -> m CNFBuilder
addLiteral lit cnf@CNFBuilder{..} =
  if | lit' == 0 || lit' > toInteger getExptdMaxVar                   ->
       throwM $ VarOutsideRange lit' getExptdMaxVar
     | getExptdClNumb == getCurrClNumb && clauseIsEmpty getCurrClause ->
       throwM $ IncorrectClauseNumber (getExptdClNumb + 1) getExptdClNumb
     | otherwise -> return . addLiteral' lit $ cnf
  where
    lit' = abs lit

{-|
adds the literal to the clause. If the literal is outside the range
denoted by the CNFBuilder, the range is increased to incorporate it
-}
addLiteral'                        :: Integer -> CNFBuilder -> CNFBuilder
addLiteral' lit cnf@CNFBuilder{..} =
  (\cnfs -> cnfs {
    getCurrClause  = clauseAddLiteral getCurrClause l,
    getExptdMaxVar = if getExptdMaxVar < l' then
                       l' else
                       getExptdMaxVar
    }) $ incrClause' cnf
  where
    l' = getWord . getVariable $ l
    l  = mkLiteralFromInteger lit
