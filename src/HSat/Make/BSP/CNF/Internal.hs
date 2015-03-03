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

mkCNFInit :: (MonadRandom m) => CNFConfig -> m CNFInit
mkCNFInit (CNFConfig
           clauseSizeBounds
           variableBounds
           clauseSizesBounds
           varsCanAppearTwice
           definitelyHasSolution) = do
  noClauses <- evalBounded clauseSizeBounds
  noVariables <- evalVariableNumber noClauses variableBounds
  clauseSizes <- replicateM (fromEnum noClauses) (evalBounded clauseSizesBounds)
  return $ CNFInit noClauses clauseSizes varsCanAppearTwice definitelyHasSolution

evalVariableNumber :: (MonadRandom m) => Word -> VariableNumber -> m Word
evalVariableNumber x _ = return x

mkCNFInit' :: (MonadRandom m) => CNFConfig -> m (CNFConfig,CNFInit)
mkCNFInit' c = do
  init <- mkCNFInit c
  return (c,init)
  
mkCNF :: (MonadRandom m) => CNFInit -> m (Either CNFMakeError CNF)
mkCNF _ = do
  return . Right . mkCNFFromClauses $ emptyClauses

mkCNF' :: (MonadRandom m) => CNFInit -> m CNF
mkCNF' init = do
  result <- mkCNF init
  return $ case result of
    Left e -> error ("Unexpected error " ++ show e)
    Right cnf -> cnf

data CNFMakeError =
  CNFMakeError
  deriving (Eq,Show)
