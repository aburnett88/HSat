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
  makeClauses -- :: (MonadRandom m) => [Word] -> Bool -> LiteralMake m Clauses
  ) where

import Control.Monad.Random
import HSat.Make.Instances.Common.Clause
import HSat.Make.Instances.Common.Literal
import HSat.Problem.Instances.Common
import Control.Monad.Catch

{-|
Given a list of 'Words' (that correspond t the size of the 'Clause'
and a 'LiteralPredicate', creates a list of 'Clause' that corresponds
to these predicates
-}
makeClauses              :: (MonadRandom m, MonadThrow m) => [Word] -> Bool ->
                            LiteralMake m Clauses
makeClauses ls predicate =
  (if predicate then makeClausesOneTrueInEach
   else makeClausesUndefined) ls emptyClauses

makeClausesUndefined           :: (MonadRandom m, MonadThrow m) => [Word] -> Clauses ->
                                  LiteralMake m Clauses
makeClausesUndefined [] cl = return cl
makeClausesUndefined (x:xs) cl = do
  (_,clause) <- makeClause False x
  makeClausesUndefined xs (clausesAddClause cl clause)

makeClausesOneTrueInEach           :: (MonadRandom m, MonadThrow m) => [Word] -> Clauses ->
                                      LiteralMake m Clauses
makeClausesOneTrueInEach [] cl     = return cl
makeClausesOneTrueInEach (x:xs) cl = do
  (_,clause) <- makeClause True x
  makeClausesOneTrueInEach xs (clausesAddClause cl clause)
