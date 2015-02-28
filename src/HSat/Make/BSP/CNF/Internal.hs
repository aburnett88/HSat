module HSat.Make.BSP.CNF.Internal (
  mkCNFInit,
  mkCNFInit',
  CNFInit(..)
  ) where

import Data.Word
import HSat.Make.Config
import HSat.Make.Internal
import Control.Monad (replicateM)
import Control.Monad.Random

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
evalVariableNumber = undefined

mkCNFInit' :: (MonadRandom m) => CNFConfig -> m (CNFInit,CNFConfig)
mkCNFInit' = undefined
  
