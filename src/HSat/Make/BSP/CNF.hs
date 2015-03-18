module HSat.Make.BSP.CNF (
  makeCNF,
  makeCNFSolution,
  makeCNF'
  ) where

import HSat.Problem.BSP.CNF
import HSat.Make.BSP.CNF.Internal
import HSat.Problem.BSP.Common
import Data.Map (Map)
import qualified Data.Map as M
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
makeCNFSolution config = do
  (_,initial) <- mkCNFInit' config
  cnf <- mkCNF' initial
  return (M.empty, cnf)
