module HSat.Make.BSP.Common.Clauses (
  makeClauses
  ) where

import Control.Monad.Random
import HSat.Problem.BSP.Common
import HSat.Make.BSP.Common.Clause
import Data.Word
import HSat.Make.BSP.Common.Literal

makeClauses :: (MonadRandom m) => [Word] -> LiteralPredicate ->
               LiteralMake m Clauses
makeClauses ls predicate =
  (case predicate of
    None -> makeClausesUndefined
    All  -> makeClausesOneTrueInEach
    Any  -> makeClausesAtleastOneAllTrue)
    ls emptyClauses

makeClausesUndefined :: (MonadRandom m) => [Word] -> Clauses ->
                        LiteralMake m Clauses
makeClausesUndefined [] cl = return cl
makeClausesUndefined (x:xs) cl = do
  (_,clause) <- makeClause None x
  makeClausesUndefined xs (clausesAddClause cl clause)

makeClausesOneTrueInEach :: (MonadRandom m) => [Word] -> Clauses ->
                            LiteralMake m Clauses
makeClausesOneTrueInEach [] cl = return cl
makeClausesOneTrueInEach (x:xs) cl = do
  (_,clause) <- makeClause Any x
  makeClausesOneTrueInEach xs (clausesAddClause cl clause)

makeClausesAtleastOneAllTrue :: (MonadRandom m) => [Word] -> Clauses ->
                                LiteralMake m Clauses
makeClausesAtleastOneAllTrue [] cl = return cl
makeClausesAtleastOneAllTrue [x] cl = do
  (_,clause) <- makeClause All x
  return (clausesAddClause cl clause)
makeClausesAtleastOneAllTrue (x:xs) cl = do
  (allTrue,clause) <- makeClause None x
  if allTrue then
    makeClausesUndefined xs (clausesAddClause cl clause) else
    makeClausesAtleastOneAllTrue xs (clausesAddClause cl clause)
