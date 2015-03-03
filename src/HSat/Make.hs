{-|
Module      : HSat.Make
Description : The Make module 
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides access to random generation of 'Problem's
-}

module HSat.Make (
  make,    -- :: (MonadRandom m) => Config -> m Problem
  makeList, -- :: (MonadRandom m) = Int -> Config -> m [Problem]
  MakeError(..)
  ) where

import Control.Monad.Random.Class
import HSat.Make.Config
import HSat.Problem
import HSat.Problem.ProblemExpr
import HSat.Problem.Source
import Control.Monad (replicateM,liftM)
import HSat.Make.BSP.CNF
import Data.Bifunctor
import HSat.Make.BSP.CNF.Internal (CNFMakeError(..))
import HSat.Problem.BSP.CNF

data MakeError =
  CNFError CNFMakeError
  deriving (Eq,Show)

liftCNF :: (Either CNFMakeError CNF) -> (Either MakeError ProblemExpr)
liftCNF = bimap CNFError mkCNFProblem

{-|
Creates a random 'Problem' from a 'Config'
-}
make :: (MonadRandom m) => Config -> Bool -> m (Either MakeError Problem)
make config ignoreErrors = do
  problemExpr <-
    case getInputConfig config of
      CNFProblemType cnfConfig -> liftCNF `liftM` if ignoreErrors then
                                    (Right . snd) `liftM` makeCNF' cnfConfig else
                                    makeCNF cnfConfig
  return $ second (makeProblemFromExpr config) problemExpr

makeProblemFromExpr :: Config -> ProblemExpr -> Problem
makeProblemFromExpr config =
  mkProblem (mkMakeConfig config) . changeProblemType (getOutputType config)

{-| Creates a list of Problem's from a given 'Config' -}

makeList :: (MonadRandom m) => Int -> Config -> m [Problem]
makeList number config =
  map (\either ->
        case either of
          Left _ -> error "makeList error"
          Right problem -> problem)
  `liftM` replicateM number (make config True)
