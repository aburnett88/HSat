module HSat.Make.BSP.Common.Clauses (
  makeClauses
  ) where

import Control.Monad.Trans.Either
import Control.Monad.State
import Control.Monad.Random
import HSat.Problem.BSP.Common

type ClausesStatusErr random result =
  StateT ClausesMakeStatus (EitherT ClausesMakeError random) result

makeClauses :: (MonadRandom m) => ClausesStatusErr m Clauses
makeClauses = undefined

data ClausesMakeStatus = ClausesMakeStatus
  deriving (Eq,Show)

data ClausesMakeError = ClausesMakeError
  deriving (Eq,Show)
