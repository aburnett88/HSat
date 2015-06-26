{-# LANGUAGE
    MultiParamTypeClasses
    #-}

{-|
Module      : HSat.Make.Instances.CNF
Description : High level functions for creating CNF files
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exports high level functions for creating 'CNF' files
-}

module HSat.Make.Instances.CNF (
  makeCNF        , -- :: (MonadRandom m, MonadThrow m) => CNFConfig -> m CNF
  makeCNFSolution, -- :: (MonadRandom m, MonadCatch m) => CNFConfig -> m (CNFConfig,CNF)
  makeCNF'       , -- :: (MonadRandom m, MonadCatch m) => CNFConfig -> m (BoolSolution,CNF)
  CNFConfig(..)  , 
  ) where

import Control.Monad.Catch
import Control.Monad.Random
import HSat.Make.Config.Class
import HSat.Make.Instances.CNF.Internal
import HSat.Problem.Instances.CNF
import HSat.Solution.Instances.CNF

instance Makeable CNFConfig CNF where
  makeProblem      = makeCNF
  makeNoErrors     = makeCNF'
  makeWithSolution = makeCNFSolution

{-|
Creates a CNF from the CNFConfig. Will throw an error if there is one
-}
makeCNF        :: (MonadRandom m, MonadThrow m) => CNFConfig -> m CNF
makeCNF config =
  mkCNFInit config >>= mkCNF
  
{-|
If a config is incorrect, will fix the config and return it and the new CNF
-}
makeCNF'        :: (MonadRandom m, MonadCatch m) =>  CNFConfig -> m (CNFConfig,CNF)
makeCNF' config = do
  (config',initial) <- mkCNFInit' config
  cnf <- mkCNF' initial
  return (config',cnf)

{-|
Makes a solution from a 'CNFConfig
-}
makeCNFSolution        :: (MonadRandom m, MonadCatch m) => CNFConfig -> m (BoolSolution,CNF)
makeCNFSolution config = do
  (_,initial) <- mkCNFInit' config
  cnf <- mkCNF' initial
  return (emptySolution, cnf)
