module HSat.Make.BSP.CNF (
  makeCNF,
  makeCNFSolution,
  makeCNF'
  ) where

import HSat.Make.BSP.Common.Literal
import HSat.Problem.BSP.CNF
import HSat.Make.BSP.CNF.Internal
import Control.Monad.Trans.Either
import Control.Monad.State
import HSat.Make.BSP.Common.Clauses
import HSat.Problem.BSP.Common
import Data.Map (Map)
import Control.Monad.Random

makeCNF :: (MonadRandom m) => CNFInit -> m (Either LiteralMakeError CNF)
makeCNF init = do
  let maxVar = getSetMaxVar init
      sizes  = getSizes init
      varsCanAppearTwice = getVarsCanAppearTwice init
      solvable = getWillBeSolvable init
  literalSet <- mkLiteralSet maxVar varsCanAppearTwice
  let predicate = if solvable then All else None
  clauses <- runEitherT (runStateT (makeClauses sizes predicate) literalSet)
  return $ fmap (mkCNFFromClauses . fst) clauses

makeCNFSolution :: (MonadRandom m) => CNFInit -> m (Either LiteralMakeError (Map Variable Sign,CNF))
makeCNFSolution = undefined

makeCNF' :: (MonadRandom m) => CNFInit -> m CNF
makeCNF' init = do
  cnf <- makeCNF init
  return $ case cnf of
    Left e -> error ("makeCNF': " ++ show e)
    Right cnf -> cnf
