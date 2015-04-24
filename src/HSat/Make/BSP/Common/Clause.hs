module HSat.Make.BSP.Common.Clause (
  makeClause
  ) where

import Control.Monad.State
import Control.Monad.Random
import HSat.Problem.BSP.Common
import HSat.Make.BSP.Common.Literal

makeClause :: (MonadRandom m) =>
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

makeClauseAllTrue :: (MonadRandom m) => Word -> Clause ->
                     LiteralMake m Clause
makeClauseAllTrue 0 c = return c
makeClauseAllTrue n c = do
  l <- getTrueLiteral
  makeClauseAllTrue (n-1) (clauseAddLiteral c l)

makeClauseUndefined :: (MonadRandom m) => Word -> Clause ->
                       LiteralMake m Clause
makeClauseUndefined 0 c = return c
makeClauseUndefined n c = do
  l <- getRandomLiteral
  makeClauseUndefined (n-1) (clauseAddLiteral c l)

makeClauseOneTrue :: (MonadRandom m) => Word -> Clause ->
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
