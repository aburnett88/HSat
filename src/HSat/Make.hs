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
  makeList -- :: (MonadRandom m) = Int -> Config -> m [Problem]
  ) where

import Control.Monad.Random.Class
import HSat.Make.Config
import HSat.Problem
import HSat.Problem.ProblemExpr
import HSat.Problem.Source
import Control.Monad (replicateM)

{-|
Creates a random 'Problem' from a 'Config'
-}
make :: (MonadRandom m) => Config -> m Problem
make config = do
  expr <-
    case getInputConfig config of
      CNFProblemType cnfConfig -> undefined
  return . mkProblem mkStatic . changeProblemType (getOutputType config) $ expr

{-|
Creates a list of Problem's from a given 'Config'
-}
makeList :: (MonadRandom m) => Int -> Config -> m [Problem]
makeList number config = replicateM number (make config)
