{-# LANGUAGE RecordWildCards #-}

module HSat.Make.BSP.CNF.Internal (
  mkCNFInit,
  mkCNFInit',
  CNFInit(..),
  CNFMakeError(..),
  mkCNF,
  mkCNF'
  ) where

import Data.Word
import HSat.Make.Config
import HSat.Make.Internal
import Control.Monad (replicateM)
import Control.Monad.Random
import HSat.Problem.BSP.CNF
import HSat.Problem.BSP.Common

data CNFInit = CNFInit {
  getSetMaxVar :: Word,
  getSizes  :: [Word],
  getVarsCanAppearTwice :: Bool,
  getWillBeSolvable :: Bool
  } deriving (Eq,Show)

{-|
Takes a 'CNFConfig' and, in a 'Random' context, creates a 'CNFInit' data
type within the bounds set out by the 'CNFConfig' type.
-}
mkCNFInit                  :: (MonadRandom m) => CNFConfig -> m CNFInit
mkCNFInit (CNFConfig {..}) = do
  numbClauses    <- evalBounded getClauseSizeBounds
  numbVariables  <- evalVariableNumber numbClauses getVariableBounds
  sizesOfClauses <- replicateM
                      (fromEnum numbClauses) $
                      evalBounded getClauseSizeBounds
  return $ CNFInit
    numbVariables sizesOfClauses
    getVarsCanAppearTwice getDefinitelyHasSolution

evalVariableNumber :: (MonadRandom m) => Word -> VariableNumber -> m Word
evalVariableNumber x _ = return x

mkCNFInit' :: (MonadRandom m) => CNFConfig -> m (CNFConfig,CNFInit)
mkCNFInit' c = do
  initial <- mkCNFInit c
  return (c,initial)
  
mkCNF :: (MonadRandom m) => CNFInit -> m (Either CNFMakeError CNF)
mkCNF _ =
  return . Right . mkCNFFromClauses $ emptyClauses

mkCNF' :: (MonadRandom m) => CNFInit -> m CNF
mkCNF' initial = do
  result <- mkCNF initial
  return $ case result of
    Left e -> error ("Unexpected error " ++ show e)
    Right cnf -> cnf

data CNFMakeError =
  CNFMakeError
  deriving (Eq,Show)
