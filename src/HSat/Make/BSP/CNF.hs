module HSat.Make.BSP.CNF (
  makeCNF,
  makeCNFSolution,
  makeCNF'
  ) where

import HSat.Problem.BSP.CNF
import HSat.Make.BSP.CNF.Internal
import HSat.Problem.BSP.Common
import Data.Map (Map)
import Control.Monad.Random
import HSat.Make.Config


makeCNF :: (MonadRandom m) => CNFConfig -> m (Either CNFMakeError CNF)
makeCNF config =
  mkCNFInit config >>= mkCNF

makeCNF' :: (MonadRandom m) =>
            CNFConfig ->
            m (CNFConfig,CNF)
makeCNF' config = do
  (config',initial) <- mkCNFInit' config
  cnf <- mkCNF' initial
  return (config',cnf)

makeCNFSolution :: (MonadRandom m) =>
                   CNFConfig ->
                   m (Map Variable Sign,CNF)
makeCNFSolution = undefined

{-

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
-}
