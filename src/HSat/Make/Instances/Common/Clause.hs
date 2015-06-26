{-|
Module      : HSat.Make.Instances.Common.Clause
Description : Provides functionality for creating random clauses
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports a function for creating random 'Clause'
-}

module HSat.Make.Instances.Common.Clause (
  makeClause -- :: (MonadRandom m) => LiteralPredicate -> Word -> LiteralMake m (Bool,Clause)
  ) where

import Control.Monad.Random
import Control.Monad.State
import HSat.Make.Instances.Common.Literal
import HSat.Problem.Instances.Common

{-|
Given a 'LiteralPredicate', a 'Word' denoting the size of the 'Clause',
this returns a tuple of a 'Bool' and a 'Clause' in a 'LiteralMake' area

The Bool denotes whether all the 'Literal's generated will evaluate to 'True'
-}
makeClause                :: (MonadRandom m) =>
                             LiteralPredicate -> Word ->
                             LiteralMake m (Bool,Clause)
makeClause predicate size = do
  clause <- (case predicate of
    Any -> makeClauseOneTrue
    All -> makeClauseAllTrue
    None -> makeClauseUndefined)
            size emptyClause
  hasGeneratedTrue <- gets getHasGeneratedTrue
  let allTrue = hasGeneratedTrue == size
  reset
  return (allTrue,clause)

makeClauseAllTrue     :: (MonadRandom m) => Word -> Clause ->
                         LiteralMake m Clause
makeClauseAllTrue 0 c = return c
makeClauseAllTrue n c = do
  l <- getTrueLiteral
  makeClauseAllTrue (n-1) (clauseAddLiteral c l)

makeClauseUndefined     :: (MonadRandom m) => Word -> Clause ->
                           LiteralMake m Clause
makeClauseUndefined 0 c = return c
makeClauseUndefined n c = do
  l <- getRandomLiteral
  makeClauseUndefined (n-1) (clauseAddLiteral c l)

makeClauseOneTrue     :: (MonadRandom m) => Word -> Clause ->
                         LiteralMake m Clause
makeClauseOneTrue 0 c = return c
makeClauseOneTrue 1 c = do
  hasGeneratedTrue <- gets getHasGeneratedTrue
  l <- case hasGeneratedTrue of
    0 -> getTrueLiteral
    _ -> getRandomLiteral
  return (clauseAddLiteral c l)
makeClauseOneTrue n c = do
  l <- getRandomLiteral
  makeClauseOneTrue (n-1) (clauseAddLiteral c l)
