{-|
Module      : HSat.Make.Instances.Common.Clauses
Description : Exports functionality to create random 'Clauses'
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports functionality to create random 'Clauses'
-}

module HSat.Make.Instances.Common.Clauses (
  makeClauses -- :: (MonadRandom m) => [Word] -> LiteralPredicate -> LiteralMake m Clauses
  ) where

import Control.Monad.Random
import HSat.Make.Instances.Common.Clause
import HSat.Make.Instances.Common.Literal
import HSat.Problem.Instances.Common

{-|
Given a list of 'Words' (that correspond t the size of the 'Clause'
and a 'LiteralPredicate', creates a list of 'Clause' that corresponds
to these predicates
-}
makeClauses              :: (MonadRandom m) => [Word] -> LiteralPredicate ->
                            LiteralMake m Clauses
makeClauses ls predicate =
  (case predicate of
    None -> makeClausesUndefined
    All  -> makeClausesOneTrueInEach
    Any  -> makeClausesAtleastOneAllTrue)
    ls emptyClauses

makeClausesUndefined           :: (MonadRandom m) => [Word] -> Clauses ->
                                  LiteralMake m Clauses
makeClausesUndefined [] cl = return cl
makeClausesUndefined (x:xs) cl = do
  (_,clause) <- makeClause None x
  makeClausesUndefined xs (clausesAddClause cl clause)

makeClausesOneTrueInEach           :: (MonadRandom m) => [Word] -> Clauses ->
                                      LiteralMake m Clauses
makeClausesOneTrueInEach [] cl     = return cl
makeClausesOneTrueInEach (x:xs) cl = do
  (_,clause) <- makeClause Any x
  makeClausesOneTrueInEach xs (clausesAddClause cl clause)

makeClausesAtleastOneAllTrue          :: (MonadRandom m) => [Word] -> Clauses ->
                                         LiteralMake m Clauses
makeClausesAtleastOneAllTrue [] cl     = return cl
makeClausesAtleastOneAllTrue [x] cl    = do
  (_,clause) <- makeClause All x
  return (clausesAddClause cl clause)
makeClausesAtleastOneAllTrue (x:xs) cl = do
  (allTrue,clause) <- makeClause None x
  if allTrue then
    makeClausesUndefined xs (clausesAddClause cl clause) else
    makeClausesAtleastOneAllTrue xs (clausesAddClause cl clause)
