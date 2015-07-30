{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies
    #-}

{-|
Module      : HSat.Solution.Instances.CNF
Description : Solution instances for CNF data type
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports functionality for a solution type for CNF
-}

module HSat.Solution.Instances.CNF (
  checkCNFSolution, -- :: CNF -> BoolSolution -> Bool
  emptySolution   , -- :: BoolSolution
  BoolSolution(..),
  mkTrueSet       , -- :: (MonadRandom m) => Word -> BoolSolution
  lookup          , -- :: Variable -> BoolSolution -> Maybe Sign
  varsEvalToTrue  ,-- :: Clause -> BoolSolution -> Word
  ) where

import Prelude hiding (lookup)
import Data.Map                      (Map)
import qualified Data.Map            as M
import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.Common
import HSat.Solution.Class
import Control.Monad.Random
import Control.Monad (replicateM)

instance Solution CNF where
  type SolInstance CNF = BoolSolution
  checkSolution        = checkCNFSolution

{-|
Checks a CNF against a solution and returns whether it is satisfied or not
-}
checkCNFSolution     :: CNF -> BoolSolution -> Bool
checkCNFSolution _ _ = error "Solution.Instance.CNF:checkNFSolution not written"

{-|
A 'BoolSolution' consists of an internal 'Map' from 'Variable's to 'Sign's
-}
data BoolSolution = BoolSolution {
  solution :: Map Variable Sign
  } deriving (Eq,Show)

{-|
Returns an empty Solution
-}
emptySolution :: BoolSolution
emptySolution = BoolSolution M.empty

mkTrueSet :: (MonadRandom m) => Word -> m BoolSolution
mkTrueSet w = do
  BoolSolution . M.fromList . zip varList <$> (replicateM (fromEnum w) getRandom)
  where
    varList = if w == 0 then [] else map mkVariable [1 .. w]

lookup :: Variable -> BoolSolution -> Maybe Sign
lookup w b = M.lookup w $ solution b


varsEvalToTrue            :: Clause -> BoolSolution -> Word
varsEvalToTrue cl boolSol = error ("Clause/varsEvalToTrue not written" ++ show boolSol ++ show cl)

solutionFromList :: [Bool] -> BoolSolution
solutionFromList = error "unwritten solutionFromList"
