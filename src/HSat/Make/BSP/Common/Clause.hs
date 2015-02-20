module HSat.Make.BSP.Common.Clause (
  makeClause,
  ClauseMakeError(..),
  ClauseMakeStatus(..)
  ) where

import Data.Word
import Control.Monad.Trans.Either
import Control.Monad.State
import Control.Monad.Random
import HSat.Problem.BSP.Common
import HSat.Make.BSP.Common.Literal

type ClauseStatusError random result =
  StateT ClauseMakeStatus (EitherT ClauseMakeError random) result

data ClauseMakeError =
  LitMakeError LiteralMakeError
  deriving (Eq,Show)

data ClauseMakeStatus = ClauseMakeStatus {
  getMustContainOneOfSet :: LiteralSet       ,
  getLiteralMakeStatus   :: LiteralMakeStatus
  } deriving (Eq,Show)

type LiteralSet = (Maybe ())

makeClause :: (MonadRandom m) => Word -> ClauseStatusError m Clause
makeClause size = do
  getMustContainOneOfSet <- gets getMustContainOneOfSet
  case getMustContainOneOfSet of
    Nothing -> makeClauseNoSolution size emptyClause
    Just sol -> makeClauseSolution size emptyClause False

makeClauseNoSolution :: (MonadRandom m) => Word -> Clause -> ClauseStatusError m Clause
makeClauseNoSolution 0 clause = return clause
makeClauseNoSolution n clause = do
  makeLiteral' >>= makeClauseNoSolution (n-1) . clauseAddLiteral clause

makeLiteral' :: ClauseStatusError m Literal
makeLiteral' = undefined

makeClauseSolution :: (MonadRandom m) => Word -> Clause -> Bool -> ClauseStatusError m Clause
makeClauseSolution = undefined
